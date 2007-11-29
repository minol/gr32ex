unit GR_ThemeForm;

interface

{$I Setting.inc}

{$Define debug}
uses
  {$ifdef Debug}
  DbugIntf,
  {$endif} 
  Windows, Messages, 
  SysUtils, Classes, 
  Graphics, Controls, Forms
  ImgList,
  Menus,
  , GR32
  , GR32_Blend
  , GR32_Image
  //,GR_ImageEx
  {$IFDEF MSWINDOWS}
  , GR_WinAPI
  {$ENDIF}
  , GR_Graphics
  , GR_Controls
  , GR_StdCtrls
  , GR_Forms
  ;

type
  { Summary the abstract layered form with Theme supports. }
  { Description
  can Load/save the form configure(Theme).
    Theme Load
    删除（暂缓）
    Save, SaveAs
    Clone
    驻留内存
  
  About Theme: it's a configure file in fact.
  
  弹出菜单选用什么？
  TTBXPopupMenu？
  TBX2.2 Alpha 和TBX2.1Beta 用哪一个？
  TBX2.1成熟点，有补丁，用它吧？
  
  配置属性以 Form 出版属性的形式实现！！
  
  the configure file structure:
  {App}\Themes\aTheme.xml
  <language name=""/>
  <Theme name="" class="">
    <Caption/>
    <Author/>
    <Email/>
    <HomePage/>
    <Hint/> the description
    <Left/>
    <Right/>
  </Theme>
  }
  TGRCustomThemeForm = class(TGRControlForm)
  private
    { Summary the theme author }
    FAuthor: string;
    { Summary the theme author's email }
    FEmail: string;
    { Summary the theme author's homepage }
    FHomePage: string;
    FMainImageList: TCustomImageList;
    FMainPopupMenu: TPopupMenu;
  protected
    procedure AssignMainPanel(const Value: TGRCustomControl); override;
    procedure DoPopupMenu(Sender: TObject);
    procedure InitPopupMenu(PopupComponent: TComponent); virtual;
    procedure UnAssignMainPanel(const Value: TGRCustomControl); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class function GetImageListClass: TImageListClass; virtual;
    class function GetPopupMenuClass: TPopupMenuClass; virtual;
    property MainImageList: TCustomImageList read FMainImageList;
    property MainPopupMenu: TPopupMenu read FMainPopupMenu;
  published
    { Summary the theme author }
    property Author: string read FAuthor write FAuthor;
    { Summary the theme author's email }
    property Email: string read FEmail write FEmail;
    { Summary the theme author's homepage }
    property HomePage: string read FHomePage write FHomePage;
  end;
  

implementation

constructor TGRCustomThemeForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMainPopupMenu := GetPopupMenuClass.Create(Self);
  FMainImageList := GetImageListClass.Create(Self);
  
  FMainPopupMenu.ImageList := FMainImageList;
  FMainPopupMenu.OnPopup := DoPopupMenu;
end;

destructor TGRCustomThemeForm.Destroy;
begin
  FreeAndNil(FMainPopupMenu);
  FreeAndNil(FMainImageList);
  inherited Destroy;
end;

procedure TGRCustomThemeForm.AssignMainPanel(const Value: TGRCustomControl);
begin
  inherited AssignMainPanel(Value);
  Value.PopupMenu := FMainPopupMenu;
end;

procedure TGRCustomThemeForm.DoPopupMenu(Sender: TObject);
begin
  if Sender is TPopupMenu then
    with Sender as TPopupMenu do
    begin
      Items.Clear;
      InitPopupMenu(PopupComponent);
    end;
end;

class function TGRCustomThemeForm.GetImageListClass: TImageListClass;
begin
  Result := TImageList;
end;

class function TGRCustomThemeForm.GetPopupMenuClass: TPopupMenuClass;
begin
  Result := TPopupMenu;
end;

procedure TGRCustomThemeForm.InitPopupMenu(PopupComponent: TComponent);
begin
end;

procedure TGRCustomThemeForm.UnAssignMainPanel(const Value: TGRCustomControl);
begin
  inherited UnAssignMainPanel(Value);
  Value.PopupMenu := nil;
end;


end.
