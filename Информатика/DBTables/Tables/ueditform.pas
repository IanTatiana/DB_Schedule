unit UEditForm;

{$mode objfpc}{$H+}{$R+}{$I+}{$M+}

interface

uses
  Classes, SysUtils, Forms, StdCtrls, UTables, Dialogs, ExtCtrls, DbCtrls,
  UMyDBTools, db, messages, Controls;

type
  FieldEdit = record
    LabName: TLabel;
    NameEng: string;
    ByName: string;
    CmbVal: TComboBox;
    EditVal: TEdit;
  end;

  TMyMessageBox = class(TForm)
    procedure Show(ACaption, AMessage: string);
    constructor CreateNew(AOwner: TComponent; Num: Integer=0); override;
  end;

  TEditForm = class(TForm)
  protected
    Table: TTable;
    BtnPanel: TPanel;
    DBTools: TMyDBTools;
    AddBtn, DelBtn, SaveBtn, CancelBtn: TButton;
    MyMessageBox: TMyMessageBox;
  public
    ID: string;
    SelectedVal: array of string;
    Fields: array of FieldEdit;
    procedure AddBtnClick(Sender: TObject);
    procedure DelBtnClick(Sender: TObject);
    procedure SaveBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    constructor CreateNew(AOwner: TComponent; Num: Integer=0); override;
    function CloseQuery: Boolean; override;
    procedure Show;
  end;

implementation

uses
  UTableForm;

//---TEditForm------------------------------------------------------------------
constructor TEditForm.CreateNew(AOwner: TComponent; Num: Integer=0);
var
  i: integer;
const
  BtnTop = 5;
  BtnHeight = 32;
begin
  inherited CreateNew(AOwner);
  Caption := 'Редактирование';
  Width := 332;
  Height := 400;
  Position := poScreenCenter;
  BorderIcons := BorderIcons - [biMaximize];
  BorderStyle := BorderStyle.bsSingle;
  Table := Tables[Num];
  MyMessageBox := TMyMessageBox.CreateNew(Self);

  DBTools := TMyDBTools.Create(AOwner, Num);
  DBTools.ExecuteQuery('SELECT COUNT(' + Table.Fields[0].en + ') FROM ' +
    Table.en);
  for i := 1 to High(Table.SelectFields) do begin
    SetLength(Fields, Length(Fields) + 1);
    Fields[High(Fields)].LabName := TLabel.Create(Self);
    Fields[High(Fields)].NameEng := Table.Fields[i].en;
    Fields[High(Fields)].ByName := ByName(Table.Fields[i].en);
    with Fields[High(Fields)].LabName do begin
      Parent := Self;
      Caption := Table.SelectFields[i].ru;
      Left := 8;
      Height := 24;
      Top := i * 16 + (i - 1)* Height;
    end;
    DBTools.ExecuteQuery(
      'SELECT ' + Table.FieldsList() +
      ' FROM ' + Table.JoinName);
    if(Table.Fields[i].ForeignKey.Table.Name = UTables.null) and
      (Table.Fields[i].DateType = int) then
    begin
      DBTools.DataSource.DataSet.First;
      Fields[High(Fields)].EditVal := TEdit.Create(AOwner);
      with Fields[High(Fields)].EditVal do begin
        Parent := Self;
        Left := 120;
        Height := 24;
        Width := 200;
        Top := i * 16 + (i - 1) * Height;
        Text := DBTools.DataSource.DataSet.Fields[i].AsString;
      end;
      Continue;
    end;
    Fields[High(Fields)].CmbVal := TComboBox.Create(AOwner);
    if Table.Fields[i].ForeignKey.Table.Name <> UTables.null then
      Fields[High(Fields)].CmbVal.Style := csDropDownList;
    with Fields[High(Fields)].CmbVal do begin
      Parent := Self;
      Left := 120;
      Height := 24;
      Width := 200;
      Top := i * 16 + (i - 1) * Height;
      DBTools.DataSource.DataSet.First;
      if Table.Fields[i].ForeignKey.Table.Name = UTables.null then
        while not DBTools.SQLQuery.EOF do
          Items.Add(DBTools.GetValue(i))
      else begin
        DBTools.ExecuteQuery('SELECT ' +
          Table.Fields[i].ForeignKey.FieldName + ' FROM ' +
          Table.Fields[i].ForeignKey.Table.Name);
        while not DBTools.SQLQuery.EOF do
          Items.AddObject(DBTools.GetValue(0), TObject(0));
      end;
    end;
  end;

  BtnPanel := TPanel.Create(Self);
  with BtnPanel do begin
    Align := Align.alBottom;
    Height := 42;
    Parent := Self;
  end;
  AddBtn := TButton.Create(BtnPanel);
  with AddBtn do begin
    Parent := BtnPanel;
    Top := BtnTop;
    Left := 8;
    Width := 80;
    Height := BtnHeight;
    Caption := 'Добавить';
    OnClick := @AddBtnClick;
  end;
  DelBtn := TButton.Create(BtnPanel);
  with DelBtn do begin
    Parent := BtnPanel;
    Top := BtnTop;
    Left := AddBtn.Left + AddBtn.Width + 4;
    Height := BtnHeight;
    Caption := 'Удалить';
    OnClick := @DelBtnClick;
  end;
  SaveBtn := TButton.Create(BtnPanel);
  with SaveBtn do begin
    Parent := BtnPanel;
    Top := BtnTop;
    Left := DelBtn.Left + DelBtn.Width + 4;
    Height := BtnHeight;
    Caption := 'Сохранить';
    OnClick := @SaveBtnClick;
  end;
  CancelBtn := TButton.Create(BtnPanel);
  with CancelBtn do begin
    Parent := BtnPanel;
    Top := BtnTop;
    Left := SaveBtn.Left + SaveBtn.Width + 4;
    Height := BtnHeight;
    Caption := 'Отмена';
    OnClick := @CancelBtnClick;
  end;
end;

procedure TEditForm.Show;
var
  i: integer;
begin
  for i := 0 to High(SelectedVal) do begin
    if Fields[i].EditVal <> nil then continue;
    Fields[i].CmbVal.Text := SelectedVal[i];
  end;
  inherited Show;
end;

function TEditForm.CloseQuery: boolean;
begin
  SetLength(SelectedVal, 0);
  inherited CloseQuery;
end;

procedure TEditForm.AddBtnClick(Sender: TObject);
var
  i: integer;
  Lim: string;
  Val: string;
begin
  Lim := ' NEXT VALUE FOR ' + Table.GenName;
  for i := 0 to High(Fields) do
    Lim := Lim + ', :param' + IntToStr(i);
  with DBTools.SQLQuery do begin
    Active := False;
    Close;
    SQL.Text := 'INSERT INTO ' + Table.en + ' VALUES(' + Lim + ' );';
  end;

  for i := 0 to High(Fields) do begin
    Case Table.TableType of
      tCascade, tMarker: begin
        if Fields[i].CmbVal <> nil then
          Val := Fields[i].CmbVal.Text;
        if Fields[i].EditVal <> nil then
          Val := Fields[i].EditVal.Text;
      end;
      tJoin: begin
        Val := IntToStr(Fields[i].CmbVal.ItemIndex + 1);
      end;
      tUnChange: begin
        ShowMessage('Данная таблица не подлежит редактированию!');
        Exit;
      end;
    end;
    DBTools.SQLQuery.Params.ParamByName('param' + IntToStr(i)).AsString := Val;
  end;

  DBTools.SQLQuery.ExecSQL;
  SQLTransaction_.Commit;
  Close;
end;

procedure TEditForm.DelBtnClick(Sender: TObject);
begin
  with DBTools.SQLQuery do begin
    Active := False;
    SQL.Clear;
    SQL.Text := 'DELETE FROM ' + Table.en + ' WHERE ' + Table.Fields[0].en +
      ' = ' + ID;
  end;

  try
    DBTools.SQLQuery.Active := True;
  except
    on E: EDatabaseError do begin
      Case MessageDlg(PChar(E.Message),
        'Вы уверены, что хотите удалить запись?'#10#13 +
        'После удаления будут утеряны все данные, ссылающиеся на эту запись',
        mtConfirmation, mbYesNo, 0) of
        mrYes: DBTools.SQLQuery.ExecSQL;
        mrNo: Exit;
      end;
    end;
  end;
  SQLTransaction_.Commit;
  Close;
end;

procedure TEditForm.SaveBtnClick(Sender: TObject);
var
  i: integer;
  Lim, Val: string;
begin
  Lim := Fields[0].NameEng + ' = :param0';
  for i := 1 to High(Fields) do
    Lim := Lim + ', ' + Fields[i].NameEng + ' = :param' + IntToStr(i);

  with DBTools.SQLQuery do begin
    Active := False;
    Close;
    SQL.Text := 'UPDATE ' + Table.en + ' SET ' + Lim + ' WHERE ' +
      Table.Fields[0].en + ' = ' + ID;
  end;

  for i := 0 to High(Fields) do begin
    Case Table.TableType of
      tCascade, tMarker, tUnChange: begin
        if Fields[i].CmbVal <> nil then
          Val := Fields[i].CmbVal.Text;
        if Fields[i].EditVal <> nil then
          Val := Fields[i].EditVal.Text;
      end;
      tJoin: Val := IntToStr(Fields[i].CmbVal.ItemIndex + 1);
    end;
    DBTools.SQLQuery.ParamByName('param' + IntToStr(i)).AsString := Val;
  end;

  DBTools.SQLQuery.ExecSQL;
  SQLTransaction_.Commit;
  Close;
end;

procedure TEditForm.CancelBtnClick(Sender: TObject);
begin
  Close;
end;

//----TMyMessageBox-------------------------------------------------------------
constructor TMyMessageBox.CreateNew(AOwner: TComponent; Num: Integer=0);
begin
  Inherited CreateNew(AOwner);
  //Caption := '';
end;

procedure TMyMessageBox.Show(ACaption, AMessage: string);
begin
  //
end;

end.

