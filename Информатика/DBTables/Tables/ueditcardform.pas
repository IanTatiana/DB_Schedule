unit ueditcardform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, UTables, UMyDBTools;

type
  TCardType = (ctAddition, ctDeletion, ctEditing);

  TEditPanel = class(TPanel)
  protected
    FLabel: array of TLabel;
    FComboBoxes: array of TComboBox;
    FTable: TTable;
    FCardType: TCardType;
    //DBTools: TMyDBTools;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TEditPanelClases = class of TEditPanel;

  TAddPanel = class(TEditPanel)
  protected
    AddBtn: TButton;
  public
    procedure AddBtnClick(Sender: TObject);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TDelPanel = class(TEditPanel)
  protected
    DelBtn: TButton;
  public
    procedure DelBtnClick(Sender: TObject);
    constructor Create(AOwner: TComponent); override;
  end;

  TUpdatePanel = class(TEditPanel)
  protected
    UpdateBtn: TButton;
  public
    //constructor Create(AOwner: TComponent); override;
  end;

  TEditCard = class(TForm)
  protected
    FCardType: TCardType;
    FEditPanel: TEditPanel;
  public
    constructor CreateNew(AOwner: TComponent; Num: Integer=0); override;
    procedure Show(ANum: integer; ACardType: TCardType);
  end;

procedure RegisterEditPanelClass(AEditPanelClass: TEditPanelClases);

var
  EditCardForm: TEditCard;
  EditPanelClassArray: array of TEditPanelClases;
  TableNum: integer;
  SelSchElem: array of string;
  ID: string;

implementation

uses
  USchedule;

procedure RegisterEditPanelClass(AEditPanelClass: TEditPanelClases);
begin
  SetLength(EditPanelClassArray, Length(EditPanelClassArray) + 1);
  EditPanelClassArray[High(EditPanelClassArray)] := AEditPanelClass;
  RegisterClass(AEditPanelClass);
end;

//----TEditPanel----------------------------------------------------------------
constructor TEditPanel.Create(AOwner: TComponent);
var
  i: integer;
  db: TMyDBTools;
begin
  inherited Create(AOwner);
  FTable := Tables[TableNum];
  db := TMyDBTools.Create(Self, TableNum);
  db.ExecuteQuery('SELECT COUNT(' + FTable.Fields[0].en + ') FROM ' +
    FTable.en);
  Align := alClient;
  for i := 1 to High(Tables[TableNum].SelectFields) do begin
    SetLength(FLabel, Length(FLabel) + 1);
    FLabel[High(FLabel)] := TLabel.Create(Self);
    with FLabel[High(FLabel)] do begin
      Parent := Self;
      Caption := FTable.SelectFields[i].ru;
      Left := 8;
      Height := 24;
      Top := i * 16 + (i - 1)* Height;
    end;
    SetLength(FComboBoxes, Length(FComboBoxes) + 1);
    FComboBoxes[High(FComboBoxes)] := TComboBox.Create(Self);
    with FComboBoxes[High(FComboBoxes)] do begin
      Parent :=Self;
      Left := 120;
      Height := 24;
      Width := 200;
      Top := i * 16 + (i - 1) * Height;
      db.DataSource.DataSet.First;
      if FTable.Fields[i].ForeignKey.Table.Name = UTables.null then
        while not db.SQLQuery.EOF do
          Items.Add(db.GetValue(i))
      else begin
        db.ExecuteQuery('SELECT ' +
          FTable.Fields[i].ForeignKey.FieldName + ' FROM ' +
          FTable.Fields[i].ForeignKey.Table.Name + ' ORDER BY ' +
          FTable.Fields[i].ForeignKey.Table.OrderBy);
        while not db.SQLQuery.EOF do
          Items.AddObject(db.GetValue(0), TObject(0));
      end;
    end;
  end;
  db.Free; db := nil;
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
  //DBTools.Free;
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

//----TDelPanel----------------------------------------------------------
constructor TDelPanel.Create(AOwner: TComponent);
var
  i: integer;
  db1, db2: TMyDBTools;
const
  BtnOffset = 16;
  BtnHeight = 32;
begin
  inherited Create(AOwner);
  db1 := TMyDBTools.Create(AOwner, TableNum);
  db2 := TMyDBTools.Create(AOwner, TableNum);
  db1.ExecuteQuery('SELECT ' + FTable.FieldsList() +
      ' FROM ' + FTable.JoinName);
  db1.DataSource.DataSet.First;
  FTable := Tables[TableNum];
  for i := 0 to High(FComboBoxes) do begin
    FComboBoxes[i].Style := csDropDownList;
    if FTable.Fields[i].ForeignKey.Table.Name = UTables.null then
      while not db1.SQLQuery.EOF do
        FComboBoxes[i].Items.Add(db1.GetValue(i))
    else begin
      db2.ExecuteQuery('SELECT ' +
        FTable.Fields[i].ForeignKey.FieldName + ' FROM ' +
        FTable.Fields[i].ForeignKey.Table.Name + ' ORDER BY ' +
        FTable.Fields[i].ForeignKey.Table.OrderBy);
      while not db2.SQLQuery.EOF do
        FComboBoxes[i].Items.AddObject(db2.GetValue(0), TObject(0));
    end;
    FComboBoxes[i].Text := SelSchElem[i];
  end;
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

procedure TDelPanel.DelBtnClick(Sender: TObject);
var
  db: TMyDBTools;
  Table: TTable;
begin
  Table := Tables[TableNum];
  db := TMyDBTools.Create(Self, TableNum);
  with db.SQLQuery do begin
    Active := False;
    SQL.Clear;
    SQL.Text := 'DELETE FROM ' + Table.en + ' WHERE ' + Table.Fields[0].en +
      ' = ' + ID;
  end;
  db.SQLQuery.ExecSQL;   db.Free; db := nil;
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
  if ACardType = ctAddition then
    FEditPanel := TAddPanel.Create(Self);
  if ACardType = ctDeletion then
    FEditPanel := TDelPanel.Create(Self);
  FEditPanel.Parent := Self;
  inherited Show;
end;

initialization
  RegisterEditPanelClass(TAddPanel);
  RegisterEditPanelClass(TDelPanel);
  RegisterEditPanelClass(TUpdatePanel);
end.

