unit UEditCardForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, UTables, UMyDBTools;

type
  TCardType = (ctAddition, ctDeletion, ctUpdating);

  TFields = record
    FLabel: TLabel;
    FComboBox: TComboBox;
    FEdit: TEdit;
    FEngName: string;
  end;

  TEditPanel = class(TPanel)
  protected
    FFields: array of TFields;
    FTable: TTable;
    FCardType: TCardType;
    CancelBtn: TButton;
    TableNum: integer;
    FPossibleList: array of array of string;
    procedure CancelBtnClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent; Num: integer);
    destructor Destroy; override;
  end;

  TAddNewDataPanel = class(TEditPanel)
  protected
    AddBtn: TButton;
    procedure AddBtnClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent; Num: integer);
    destructor Destroy; override;
  end;

  TEditOldDataPanel = class(TEditPanel)
    constructor Create(AOwner: TComponent; Num: integer);
  end;

  TDelDataPanel = class(TEditOldDataPanel)
  protected
    DelBtn: TButton;
    procedure DelBtnClick(Sender: TObject);
  public
    destructor Destroy; override;
    constructor Create(AOwner: TComponent; Num: integer);
  end;

  TUpdateDataPanel = class(TEditOldDataPanel)
  protected
    UpdateBtn: TButton;
    procedure UpdateBtnClick(Sender: TObject);
  public
    destructor Destroy; override;
    constructor Create(AOwner: TComponent; Num: integer);
  end;

  TEditCard = class(TForm)
  protected
    FCardType: TCardType;
    FEditPanel: TEditPanel;
    TableNum: integer;
  public
    constructor CreateNew(AOwner: TComponent; Num: Integer=0); override;
    procedure Show(ANum: integer; ACardType: TCardType);
  end;

var
  EditCardForm: TEditCard;
  CallingForm: TForm;
  SelSchElem: array of string;
  ID: string;

implementation

uses
  USchedule, UTableForm;

//----TEditPanel----------------------------------------------------------------
constructor TEditPanel.Create(AOwner: TComponent; Num: integer);
var
  i: integer;
  db1: TMyDBTools;
const
  BtnOffset = 16;
  BtnHeight = 32;
begin
  inherited Create(AOwner);
  TableNum := Num;
  FTable := Tables[TableNum];
  db1 := TMyDBTools.Create(Self, TableNum);
  Align := alClient;
  for i := 1 to High(Tables[TableNum].SelectFields) do begin
    SetLength(FFields, Length(FFields) + 1);
    SetLength(FPossibleList, Length(FPossibleList) + 1);
    FFields[High(FFields)].FEngName := FTable.Fields[i].en;
    FFields[High(FFields)].FLabel := TLabel.Create(Self);
    with FFields[High(FFields)].FLabel do begin
      Parent := Self;
      Caption := FTable.SelectFields[i].ru;
      Left := 8;
      Height := 24;
      Top := i * 16 + (i - 1)* Height;
    end;
    db1.ExecuteQuery('SELECT ' + FTable.FieldsList() +
      ' FROM ' + FTable.JoinName);
    if(FTable.Fields[i].ForeignKey.Table.Name = UTables.null) and
      (FTable.Fields[i].DateType = int) then
    begin
      db1.DataSource.DataSet.First;
      FFields[High(FFields)].FEdit := TEdit.Create(AOwner);
      with FFields[High(FFields)].FEdit do begin
        Parent := Self;
        Left := 120;
        Height := 24;
        Width := 200;
        Top := i * 16 + (i - 1) * Height;
        Text := db1.DataSource.DataSet.Fields[i].AsString;
      end;
      Continue;
    end;
    FFields[High(FFields)].FComboBox := TComboBox.Create(Self);
    with FFields[High(FFields)].FComboBox do begin
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
          FPossibleList[i - 1][High(FPossibleList[i - 1])] :=
            db1.SQLQuery.Fields[0].AsString;
          db1.DataSource.DataSet.Next;
        end;
      end;
    end;
  end;
  db1.Free;
  CancelBtn := TButton.Create(Self);
  with CancelBtn do begin
    Parent := Self;
    Width := 80;
    Height := BtnHeight;
    Top := EditCardForm.Height - BtnHeight - BtnOffset;
    Left := EditCardForm.Width - Width - BtnOffset ;
    Caption := 'Отмена';
    OnClick := @CancelBtnClick;
  end;
end;

procedure TEditPanel.CancelBtnClick(Sender: TObject);
begin
  (Parent as TEditCard).Close;
end;

destructor TEditPanel.Destroy;
var
  i: integer;
begin
  for i := 0 to High(FFields) do begin
    FFields[i].FComboBox.Free; FFields[i].FComboBox := nil;
    FFields[i].FEdit.Free; FFields[i].FEdit := nil;
  end;
  SetLength(FFields, 0);
  FTable := nil;
  inherited Destroy;
end;

//----TAddNewDataPanel----------------------------------------------------------
constructor TAddNewDataPanel.Create(AOwner: TComponent; Num: integer);
var
  i: integer;
const
  BtnOffset = 16;
  BtnHeight = 32;
begin
  inherited Create(AOwner, Num);
  for i := 0 to High(FFields) do begin
    if FFields[i].FComboBox <> nil then begin
      FFields[i].FComboBox.Text := 'Выбрать из списка...';
    end;
    if FFields[i].FEdit <> nil then
      FFields[i].FEdit.Text := '';
  end;
  AddBtn := TButton.Create(Self);
  with AddBtn do begin
    Parent := Self;
    Width := 80;
    Height := BtnHeight;
    Top := EditCardForm.Height - BtnHeight - BtnOffset;
    Left := EditCardForm.Width - 2*Width - BtnOffset;
    Caption := 'Добавить';
    OnClick := @AddBtnClick;
  end;
end;

destructor TAddNewDataPanel.Destroy;
begin
  AddBtn.Free; AddBtn := nil;
  inherited Destroy;
end;

procedure TAddNewDataPanel.AddBtnClick(Sender: TObject);
var
  i: integer;
  Lim, Val: string;
  db: TMyDBTools;
begin
  db := TMyDBTools.Create(Self, TableNum);
  Lim := ' NEXT VALUE FOR ' + FTable.GenName;
  for i := 0 to High(FFields) do
    Lim := Lim + ', :param' + IntToStr(i);
  with db.SQLQuery do begin
    Active := False;
    Close;
    SQL.Text := 'INSERT INTO ' + FTable.en + ' VALUES(' + Lim + ' );';
  end;
  for i := 0 to High(FFields) do begin
    Case FTable.TableType of
      tCascade, tMarker: begin
        if FFields[i].FComboBox <> nil then
          Val := FFields[i].FComboBox.Text;
        if FFields[i].FEdit <> nil then
          Val := FFields[i].FEdit.Text;
      end;
      tJoin: begin
        Val := FPossibleList[i][FFields[i].FComboBox.ItemIndex];
      end;
      tUnChange: begin
        ShowMessage('Данная таблица не подлежит редактированию!');
        Exit;
      end;
    end;
    db.SQLQuery.Params.ParamByName('param' + IntToStr(i)).AsString := Val;
  end;
  db.SQLQuery.ExecSQL; db.Free;
  SQLTransaction_.Commit;
  SetLength(SelSchElem, 0);
  (Parent as TEditCard).Close;
end;

//----TEditOldDataPanel---------------------------------------------------------
constructor TEditOldDataPanel.Create(AOwner: TComponent; Num: integer);
var
  i: integer;
const
  BtnOffset = 16;
  BtnHeight = 32;
begin
  inherited Create(AOwner, Num);
  for i := 0 to High(FFields) do begin
    if FFields[i].FComboBox <> nil then begin
      FFields[i].FComboBox.Style := csDropDownList;
      FFields[i].FComboBox.Text := SelSchElem[i];
    end;
  end;
end;

//----TDelDataPanel-------------------------------------------------------------
constructor TDelDataPanel.Create(AOwner: TComponent; Num: integer);
const
  BtnOffset = 16;
  BtnHeight = 32;
begin
  inherited Create(AOwner, Num);
  DelBtn := TButton.Create(Self);
  with DelBtn do begin
    Parent := Self;
    Width := 80;
    Height := BtnHeight;
    Top := EditCardForm.Height - BtnHeight - BtnOffset;
    Left := EditCardForm.Width - 2*Width - BtnOffset;
    Caption := ' Удалить ';
    OnClick := @DelBtnClick;
  end;
end;

procedure TDelDataPanel.DelBtnClick(Sender: TObject);
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
  db.SQLQuery.ExecSQL; db.Free; db := nil;
  SQLTransaction_.Commit;
  SetLength(SelSchElem, 0);
  (Parent as TEditCard).Close;
end;

destructor TDelDataPanel.Destroy;
begin
  FreeAndNil(DelBtn);
  inherited Destroy;
end;

//----TUpdateDataPanel----------------------------------------------------------
constructor TUpdateDataPanel.Create(AOwner: TComponent; Num: integer);
const
  BtnOffset = 16;
  BtnHeight = 32;
begin
  inherited Create(AOwner, Num);
  UpdateBtn := TButton.Create(Self);
  with UpdateBtn do begin
    Parent := Self;
    Width := 80;
    Height := BtnHeight;
    Top := EditCardForm.Height - BtnHeight - BtnOffset;
    Left := EditCardForm.Width - 2*Width - BtnOffset;
    Caption := ' Сохранить ';
    OnClick := @UpdateBtnClick;
  end;
end;

procedure TUpdateDataPanel.UpdateBtnClick(Sender: TObject);
var
  i: integer;
  Lim, Val: string;
  db: TMyDBTools;
  Table: TTable;
begin
  Table := Tables[TableNum];
  Lim := FFields[0].FEngName + ' = :param0';
  for i := 1 to High(FFields) do
    Lim := Lim + ', ' + FFields[i].FEngName + ' = :param' + IntToStr(i);
  db := TMyDBTools.Create(Self, TableNum);
  with db.SQLQuery do begin
    Active := False; Close;
    SQL.Text := 'UPDATE ' + Table.en + ' SET ' + Lim + ' WHERE ' +
      Table.Fields[0].en + ' = ' + ID;
  end;
  for i := 0 to High(FFields) do begin
    Case Table.TableType of
      tCascade, tMarker, tUnChange: begin
        if FFields[i].FComboBox <> nil then
          Val := FFields[i].FComboBox.Text;
        if FFields[i].FEdit <> nil then
          Val := FFields[i].FEdit.Text;
      end;
      tJoin: Val := FPossibleList[i][FFields[i].FComboBox.ItemIndex];
    end;
    db.SQLQuery.ParamByName('param' + IntToStr(i)).AsString := Val;
  end;
  db.SQLQuery.ExecSQL; db.Free; db := nil;
  SQLTransaction_.Commit;
  (Parent as TEditCard).Close;
end;

destructor TUpdateDataPanel.Destroy;
begin
  FreeAndNil(UpdateBtn);
  inherited Destroy;
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
end;

procedure TEditCard.Show(ANum: integer; ACardType: TCardType);
begin
  FEditPanel.Free; FEditPanel := nil;
  if ACardType = ctAddition then
    FEditPanel := TAddNewDataPanel.Create(Self, ANum);
  if ACardType = ctDeletion then
    FEditPanel := TDelDataPanel.Create(Self, ANum);
  if ACardType = ctUpdating then
    FEditPanel := TUpdateDataPanel.Create(Self, ANum);
  FEditPanel.Parent := Self;
  inherited Show;
end;

end.

