unit ueditcardform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, UTables, UMyDBTools;

type
  TCardType = (ctAddition, ctDeletion, ctUpdating);

  TEditPanel = class(TPanel)
  protected
    FLabel: array of TLabel;
    FComboBoxes: array of TComboBox;
    FEngName: array of string;
    FTable: TTable;
    FCardType: TCardType;

    //  Список возможных значений для одной ячейки в отсортированном порядке
    FPossibleList: array of array of string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TAddPanel = class(TEditPanel)
  protected
    AddBtn: TButton;
  public
    procedure AddBtnClick(Sender: TObject);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TSelData = class(TEditPanel)
  protected
    DelBtn, UpdateBtn: TButton;
    procedure DelBtnClick(Sender: TObject);
    procedure UpdateBtnClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent; ACardType: TCardType);
  end;

  TEditCard = class(TForm)
  protected
    FCardType: TCardType;
    FEditPanel: TEditPanel;
  public
    constructor CreateNew(AOwner: TComponent; Num: Integer=0); override;
    procedure Show(ANum: integer; ACardType: TCardType);
  end;

var
  EditCardForm: TEditCard;
  TableNum: integer;
  SelSchElem: array of string;
  ID: string;

implementation

uses
  USchedule;

//----TEditPanel----------------------------------------------------------------
constructor TEditPanel.Create(AOwner: TComponent);
var
  i: integer;
  db1: TMyDBTools;
begin
  inherited Create(AOwner);
  FTable := Tables[TableNum];
  db1 := TMyDBTools.Create(Self, TableNum);
  Align := alClient;
  for i := 1 to High(Tables[TableNum].SelectFields) do begin
    SetLength(FEngName, Length(FEngName) + 1);
    SetLength(FPossibleList, Length(FPossibleList) + 1);
    FEngName[High(FEngName)] := FTable.Fields[i].en;
    SetLength(FLabel, Length(FLabel) + 1);
    FLabel[High(FLabel)] := TLabel.Create(Self);
    with FLabel[High(FLabel)] do begin
      Parent := Self;
      Caption := FTable.SelectFields[i].ru;
      Left := 8;
      Height := 24;
      Top := i * 16 + (i - 1)* Height;
    end;
    db1.ExecuteQuery('SELECT ' + FTable.FieldsList() +
      ' FROM ' + FTable.JoinName);
    SetLength(FComboBoxes, Length(FComboBoxes) + 1);
    FComboBoxes[High(FComboBoxes)] := TComboBox.Create(Self);
    with FComboBoxes[High(FComboBoxes)] do begin
      Parent :=Self;
      Left := 120;
      Height := 24;
      Width := 200;
      Top := i * 16 + (i - 1) * Height;
      db1.DataSource.DataSet.First;
      if FTable.Fields[i].ForeignKey.Table.Name = UTables.null then
        while not db1.SQLQuery.EOF do
          Items.Add(db1.GetValue(i))
      else begin
        db1.ExecuteQuery('SELECT * FROM ' +
          FTable.Fields[i].ForeignKey.Table.Name + ' ORDER BY ' +
          FTable.Fields[i].ForeignKey.Table.OrderBy);
        while not db1.SQLQuery.EOF do begin
          Items.Add(db1.SQLQuery.FieldByName(
            ByName(FTable.Fields[i].ForeignKey.FieldName)).AsString);
          SetLength(FPossibleList[i - 1], Length(FPossibleList[i - 1]) + 1);
          FPossibleList[i - 1][High(FPossibleList[i - 1])] := db1.GetValue(0);
        end;
      end;
    end;
  end;
  db1.Free; db1 := nil;
end;

destructor TEditPanel.Destroy;
var
  i: integer;
begin
  for i := 0 to High(FLabel) do begin
    FLabel[i].Free; FLabel[i] := nil;
    FComboBoxes[i].Free; FComboBoxes[i] := nil;
  end;
  SetLength(FLabel, 0);
  SetLength(FComboBoxes, 0);
  FTable := nil;
  inherited Destroy;
end;

//----TAddPanel-----------------------------------------------------------------
constructor TAddPanel.Create(AOwner: TComponent);
var
  i: integer;
const
  BtnOffset = 16;
  BtnHeight = 32;
begin
  inherited Create(AOwner);
  for i := 0 to High(FComboBoxes) do
    FComboBoxes[i].Text := 'Выбрать из списка...';
  AddBtn := TButton.Create(Self);
  with AddBtn do begin
    Parent := Self;
    Width := 80;
    Height := BtnHeight;
    Top := EditCardForm.Height - BtnHeight - BtnOffset;
    Left := EditCardForm.Width - Width - BtnOffset;
    Caption := 'Добавить';
    OnClick := @AddBtnClick;
  end;
end;

destructor TAddPanel.Destroy;
begin
  AddBtn.Free; AddBtn := nil;
  inherited Destroy;
end;

procedure TAddPanel.AddBtnClick(Sender: TObject);
var
  i: integer;
  Lim, Val: string;
  db: TMyDBTools;
begin
  db := TMyDBTools.Create(Self, TableNum);
  Lim := ' NEXT VALUE FOR ' + FTable.GenName;
  for i := 0 to High(FComboBoxes) do
    Lim := Lim + ', :param' + IntToStr(i);
  with db.SQLQuery do begin
    Active := False;
    Close;
    SQL.Text := 'INSERT INTO ' + FTable.en + ' VALUES(' + Lim + ' );';
  end;
  for i := 0 to High(FComboBoxes) do begin
    Case FTable.TableType of
      tCascade, tMarker: begin
        if FComboBoxes[i] <> nil then
          Val := FComboBoxes[i].Text;
        {if F <> nil then
          Val := Fields[i].EditVal.Text;}
      end;
      tJoin: begin
        Val := IntToStr(FComboBoxes[i].ItemIndex + 1);
      end;
      tUnChange: begin
        ShowMessage('Данная таблица не подлежит редактированию!');
        Exit;
      end;
    end;
    db.SQLQuery.Params.ParamByName('param' + IntToStr(i)).AsString := Val;
  end;
  db.SQLQuery.ExecSQL;
  db.Free; db := nil;
  SQLTransaction_.Commit;
  ScheduleTable.Show;
  (Parent as TEditCard).Close;
end;

//----TSelData------------------------------------------------------------------
constructor TSelData.Create(AOwner: TComponent; ACardType: TCardType);
var
  i: integer;
  db1, db2: TMyDBTools;
const
  BtnOffset = 16;
  BtnHeight = 32;
begin
  inherited Create(AOwner);
  for i := 0 to High(FComboBoxes) do begin
    FComboBoxes[i].Style := csDropDownList;
    FComboBoxes[i].Text := SelSchElem[i];
  end;
  if ACardType = ctDeletion then begin
    DelBtn := TButton.Create(Self);
    with DelBtn do begin
      Parent := Self;
      Width := 80;
      Height := BtnHeight;
      Top := EditCardForm.Height - BtnHeight - BtnOffset;
      Left := EditCardForm.Width - Width - BtnOffset;
      Caption := ' Удалить ';
      OnClick := @DelBtnClick;
    end;
  end;
  if ACardType = ctUpdating then begin
    UpdateBtn := TButton.Create(Self);
    with UpdateBtn do begin
      Parent := Self;
      Width := 80;
      Height := BtnHeight;
      Top := EditCardForm.Height - BtnHeight - BtnOffset;
      Left := EditCardForm.Width - Width - BtnOffset;
      Caption := ' Сохранить ';
      OnClick := @UpdateBtnClick;
    end;
  end;
end;

procedure TSelData.DelBtnClick(Sender: TObject);
var
  db: TMyDBTools;
  Table: TTable;
begin
  Table := Tables[TableNum];
  db := TMyDBTools.Create(Self, TableNum);
  with db.SQLQuery do begin
    Active := False; SQL.Clear;
    SQL.Text := 'DELETE FROM ' + Table.en + ' WHERE ' + Table.Fields[0].en +
      ' = ' + ID;
  end;
  db.SQLQuery.ExecSQL;   db.Free; db := nil;
  SQLTransaction_.Commit;
  ScheduleTable.Show;
  SetLength(SelSchElem, 0);
  (Parent as TEditCard).Close;
end;

procedure TSelData.UpdateBtnClick(Sender: TObject);
var
  i: integer;
  Lim, Val: string;
  db: TMyDBTools;
  Table: TTable;
begin
  Table := Tables[TableNum];
  Lim := FEngName[0] + ' = :param0';
  for i := 1 to High(FEngName) do
    Lim := Lim + ', ' + FEngName[i] + ' = :param' + IntToStr(i);
  db := TMyDBTools.Create(Self, TableNum);
  with db.SQLQuery do begin
    Active := False;
    Close;
    SQL.Text := 'UPDATE ' + Table.en + ' SET ' + Lim + ' WHERE ' +
      Table.Fields[0].en + ' = ' + ID;
  end;
  for i := 0 to High(FComboBoxes) do begin
    Case Table.TableType of
      tCascade, tMarker, tUnChange: begin
        if FComboBoxes[i] <> nil then
          Val := FComboBoxes[i].Text;
        {if FLabel[i] <> nil then
          Val := Fields[i].EditVal.Text;}
      end;
      tJoin: Val := FPossibleList[i][FComboBoxes[i].ItemIndex];
    end;
    db.SQLQuery.ParamByName('param' + IntToStr(i)).AsString := Val;
  end;
  ShowMessage(db.SQLQuery.SQL.Text);
  db.SQLQuery.ExecSQL;
  SQLTransaction_.Commit;
  ScheduleTable.Show;
  SetLength(SelSchElem, 0);
  (Parent as TEditCard).Close;
end;

//----TEditCard-----------------------------------------------------------------
constructor TEditCard.CreateNew(AOwner: TComponent; Num: Integer=0);
begin
  inherited CreateNew(AOwner);
  Caption := 'Редактирование';
  Width := 332;
  Height := 400;
  Position := poScreenCenter;
  BorderIcons := BorderIcons - [biMaximize];
  BorderStyle := BorderStyle.bsSingle;
  TableNum := Num;
end;

procedure TEditCard.Show(ANum: integer; ACardType: TCardType);
begin
  TableNum := ANum;
  FEditPanel.Free; FEditPanel := nil;
  if ACardType = ctAddition then
    FEditPanel := TAddPanel.Create(Self);
  if ACardType = ctDeletion then
    FEditPanel := TSelData.Create(Self, ctDeletion);
  if ACardType = ctUpdating then
    FEditPanel := TSelData.Create(Self, ctUpdating);
  FEditPanel.Parent := Self;
  inherited Show;
end;
end.

