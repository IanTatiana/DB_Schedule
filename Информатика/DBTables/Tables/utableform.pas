unit UTableForm;

{$mode objfpc}{$H+}{$M+}

interface

uses
  Classes, SysUtils, DBGrids, Forms, SQLdb, IBConnection, db, Controls,
  ExtCtrls, StdCtrls, UTables, UFilter, Dialogs, UEditForm, UMyDBTools;

type
  TTableForm = class(TForm)
  protected
    Table: TTable;
    FilterPanel: TFilterPanel;

    EditForm: TEditForm;
    DBTools: TMyDBTools;
  published
    DBGrids: TDBGrid;
    constructor CreateNew(AOwner: TComponent; Num: Integer=0);
    procedure Activate; override;
    procedure GridDblClick(Sender: TObject);
    procedure Show;
    procedure Russification;
  end;

implementation

//----TTableForm----------------------------------------------------------------
procedure TTableForm.Russification;
var
  i: integer;
begin
  for i := 0 to High(Table.SelectFields) do begin
    DBGrids.Columns[0].Visible := False;
    DBGrids.Columns[i].Title.Caption := Table.SelectFields[i].ru;
    DBGrids.Columns[i].Width := Table.SelectFields[i].Width;
  end;
end;

constructor TTableForm.CreateNew(AOwner: TComponent; Num: Integer=0);
begin
  inherited CreateNew(AOwner);
  Width := 856;
  Height := 612;
  Position := poScreenCenter;
  Table := Tables[Num];

  DBTools := TMyDBTools.Create(AOwner, Num);
  Caption := 'Таблица - ' + Table.ru;

  DBGrids := TDBGrid.Create(Self);
  with DBGrids do begin
    Parent := Self;
    Align := alClient;
    DataSource := DBTools.DataSource;
    OnDblClick := @GridDblClick;
  end;

  DBTools.ExecuteQuery('SELECT ' + Table.FieldsList() + ' FROM ' +
    Table.JoinName);
  Russification;
  FilterPanel := TFilterPanel.Create(Self, DBTools, Num);

  EditForm := TEditForm.CreateNew(Self, Num);
end;

procedure TTableForm.Activate;
begin
  DBTools.SQLQuery.Active := True;
  Russification;
  inherited;
end;

procedure TTableForm.GridDblClick(Sender: TObject);
var
  i: integer;
begin
  for i := 1 to DBGrids.Columns.Count - 1 do begin
    SetLength(EditForm.SelectedVal, Length(EditForm.SelectedVal) + 1);
    EditForm.SelectedVal[High(EditForm.SelectedVal)] :=
      DBGrids.Columns[i].Field.Text;
  end;
  EditForm.ID := DBTools.DataSource.DataSet.Fields[0].Value;
  EditForm.Show();
end;

procedure TTableForm.Show();
var
  i: integer;
begin
  Russification;
  inherited Show;
end;

end.
{
Общая панель:
1. Панель всех фильтров: Панель каждого фильтра
2. Панель кнопок
}
