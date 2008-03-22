unit ViewMain;

interface

uses
  WinTypes, WinProcs, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, Grids, Outline, CompDoc;

type
  TForm1 = class(TForm)
    MainMenu1 : TMainMenu;
    Open : TOpenDialog;
    File1 : TMenuItem;
    Open1 : TMenuItem;
    Exit1 : TMenuItem;
    Outline : TOutline;
    View1: TMenuItem;
    Expand1: TMenuItem;
    Collapse1: TMenuItem;
    procedure Exit1Click(Sender : TObject);
    procedure Open1Click(Sender : TObject);
    procedure Expand1Click(Sender: TObject);
    procedure Collapse1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure ExpandNode(Index : integer; S : TStorage);
  end;

var
  Form1 : TForm1;

implementation


{$R *.DFM}

procedure TForm1.Exit1Click(Sender : TObject);
begin
  Close;
end;

procedure TForm1.Open1Click(Sender : TObject);
var
  Name : string;
  Root : TRootStorage;
begin
  if Open.Execute then
  begin
    Name := Open.FileName;
    if (Name <> '') and FileIsCompoundDoc(Name) then
    begin
      Outline.BeginUpdate;
      Outline.Clear;
      Root := TRootStorage.Create(Name, amReadWrite, smDenyWrite, tmTransacted, false);
      try
        Outline.Add(0, Name);
        ExpandNode(1, Root);
      finally
        Root.Free;
      end;
      Outline.EndUpdate;
    end
    else
    begin
      ShowMessage('Not a compound document');
    end;
  end;
end;


procedure TForm1.ExpandNode(Index : integer; S : TStorage);
var
  storage : TStorage;
  storages : TStringList;
  streams : TStringList;
  n : integer;
  Name : string;
begin
  streams := TStringList.Create;
  try
    S.ListStreams(streams);
    if streams.Count = 0 then
    begin
      { need to add something or the storage will look like a stream }
      Outline.AddChild(Index, 'storage is empty');
    end
    else
    begin
      for n := 1 to streams.Count do
      begin
        Name := streams[n - 1];
        Outline.AddChild(Index, Name)
      end;
    end;
  finally
    streams.Free;
  end;
  inc(index);
  storages := TStringList.Create;
  try
    S.ListStorages(storages);
    begin
      for n := 1 to storages.Count do
      begin
        Name := storages[n - 1];
        storage := TStorage.Create(Name, S, amReadWrite, tmDirect, false);
        try
          Index := Outline.Add(Index, Name);
          { recursive call next }
          ExpandNode(Index, storage);
        finally
          storage.Free;
        end;
      end;
    end;
  finally
    storages.Free;
  end;
end;

procedure TForm1.Expand1Click(Sender: TObject);
begin
  Outline.FullExpand;
end;

procedure TForm1.Collapse1Click(Sender: TObject);
begin
  Outline.FullCollapse;
end;

end.

