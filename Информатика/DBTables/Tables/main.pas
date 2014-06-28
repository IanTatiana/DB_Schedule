unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Dialogs, Menus, ExtCtrls, Grids, UTables,
  UTableForm, USchedule, sqldb, IBConnection, UMyDBTools, Controls, StdCtrls,
  ComCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    DrawGrid1: TDrawGrid;
  protected
    MyDBTools: TMyDBTools;
  published
    MainMenu1: TMainMenu;
    ScheduleItem: TMenuItem;
    TablesItem: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure ScheduleItemClick(Sender: TObject);
    procedure ShowTables(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  MenuItem: TMenuItem;
  i: integer;
const
  SYSDBA = 'SYSDBA';
  masterkey = 'masterkey';
begin
  Position := poScreenCenter;
  for i := 0 to High(Tables) do begin
    MenuItem := TMenuItem.Create(MainMenu1);
    with MenuItem do begin
      Caption := Tables[i].ru;
      Name := Tables[i].en + 'TableItem';
      OnClick := @ShowTables;
      Tag := i;
    end;
    TablesItem.Insert(i, MenuItem);
  end;
  IBConnection_ := TIBConnection.Create(Self);
  with IBConnection_ do begin
    HostName := 'localhost';
    DatabaseName := 'C:\TestDB\RASPBUILD.FDB';
    UserName := SYSDBA;
    Password := masterkey;
    Transaction := SQLTransaction_;
    Connected := True;
  end;
  SQLTransaction_ := TSQLTransaction.Create(Self);
  with SQLTransaction_ do begin
    DataBase := IBConnection_;
    Active := True;
  end;
end;

procedure TForm1.ScheduleItemClick(Sender: TObject);
begin
  ScheduleTable := TScheduleTable.CreateNew(Self);
  ScheduleTable.Show;
end;

procedure TForm1.ShowTables(Sender: TObject);
var
  i, j: integer;
  s: TScreen;
  TableForm: TTableForm;
begin
  s := Screen;
  j := (Sender as TMenuItem).Tag;
  for i := 0 to Screen.FormCount - 1 do begin
    if s.Forms[i].Visible then
      if s.Forms[i].Caption = 'Таблица - ' + Tables[j].ru then begin
         s.Forms[i].Show;
         Exit;
      end;
  end;
  TableForm := TTableForm.CreateNew(Self, j);
  TableForm.Caption := 'Таблица - ' + Tables[j].ru;
  TableForm.Show;
end;

end.

