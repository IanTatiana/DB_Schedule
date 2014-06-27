unit UTableForm;

{$mode objfpc}{$H+}{$M+}

interface

uses
  Classes, SysUtils, DBGrids, Forms, SQLdb, IBConnection, db, Controls,
  ExtCtrls, StdCtrls, UTables, UFilter, Dialogs, UEditCardForm, UMyDBTools;

type
  TTableForm = class(TForm)
  protected
    Table: TTable;
    EditCardBtnPanel: TPanel;
    FilterPanel: TFilterPanel;
    DBTools: TMyDBTools;
    AddBtn, DelBtn, UpdateBtn: TButton;
    procedure UpdateBtnClick(Sender: TObject);
    procedure DelBtnClick(Sender: TObject);
    procedure AddBtnClick(Sender: TObject);
    procedure InitData();
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
  TableNum := Num;
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
  EditCardBtnPanel := TPanel.Create(Self);
  with EditCardBtnPanel do begin
    Parent := Self;
    Height := 48;
    Align := alBottom;
  end;
  UpdateBtn := TButton.Create(EditCardBtnPanel);
  with UpdateBtn do begin
    Parent := EditCardBtnPanel;
    Top := 8;
    Width := 120;
    Height := 32;
    Left := FilterPanel.Width;
    Caption := 'Изменить запись';
    OnClick := @UpdateBtnClick;
  end;
  DelBtn := TButton.Create(EditCardBtnPanel);
  with DelBtn do begin
    Parent := EditCardBtnPanel;
    Top := 8;
    Width := 120;
    Height := 32;
    Left := UpdateBtn.Left + Width;
    Caption := 'Удалить запись';
    OnClick := @DelBtnClick;
  end;
  AddBtn := TButton.Create(EditCardBtnPanel);
  with AddBtn do begin
    Parent := EditCardBtnPanel;
    Top := 8;
    Width := 120;
    Height := 32;
    Left := DelBtn.Left + Width;
    Caption := 'Добавить запись';
    OnClick := @AddBtnClick;
  end;
  EditCardForm.Free; EditCardForm := nil;
  EditCardForm := TEditCard.CreateNew(Self, Num);
end;

procedure TTableForm.InitData();
var
  i: integer;
begin
  SetLength(SelSchElem, 0);
  for i := 1 to DBGrids.Columns.Count - 1 do begin
    SetLength(SelSchElem, Length(SelSchElem) + 1);
    SelSchElem[High(SelSchElem)] := DBGrids.Columns[i].Field.Text;
  end;
  ID := DBTools.DataSource.DataSet.Fields[0].Value;
end;

procedure TTableForm.UpdateBtnClick(Sender: TObject);
begin
  InitData();
  CallingForm := Self as TTableForm;
  EditCardForm.Show(TableNum, ctUpdating);
end;

procedure TTableForm.DelBtnClick(Sender: TObject);
begin
  InitData();
  CallingForm := Self as TTableForm;
  EditCardForm.Show(TableNum, ctDeletion);
end;

procedure TTableForm.AddBtnClick(Sender: TObject);
begin
  InitData();
  CallingForm := Self as TTableForm;
  EditCardForm.Show(TableNum, ctAddition);
end;

procedure TTableForm.Activate;
begin
  DBTools.SQLQuery.Active := True;
  Russification;
  inherited;
end;

procedure TTableForm.GridDblClick(Sender: TObject);
begin
  UpdateBtnClick(Sender);
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
