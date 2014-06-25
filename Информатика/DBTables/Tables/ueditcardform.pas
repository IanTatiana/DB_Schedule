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
    Table: TTable;
    //DBTools: TMyDBTools;
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

  TEditCard = class(TForm)
  protected
    FEditPanel: TEditPanel;
  public
    constructor CreateNew(AOwner: TComponent; Num: Integer=0); override;
    procedure Show(ANum: integer; ACardType: TCardType);
  end;

var
  EditCardForm: TEditCard;
  TableNum: integer;

implementation

uses
  USchedule;

//----TEditPanel----------------------------------------------------------------
constructor TEditPanel.Create(AOwner: TComponent);
var
  i: integer;
  db: TMyDBTools;
begin
  inherited Create(AOwner);
  Table := Tables[TableNum];
  db := TMyDBTools.Create(Self, TableNum);
  db.ExecuteQuery('SELECT COUNT(' + Table.Fields[0].en + ') FROM ' +
    Table.en);
  Align := alClient;
  for i := 1 to High(Tables[TableNum].SelectFields) do begin
    SetLength(FLabel, Length(FLabel) + 1);
    FLabel[High(FLabel)] := TLabel.Create(Self);
    with FLabel[High(FLabel)] do begin
      Parent := Self;
      Caption := Table.SelectFields[i].ru;
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
      if Table.Fields[i].ForeignKey.Table.Name = UTables.null then
        while not db.SQLQuery.EOF do
          Items.Add(db.GetValue(i))
      else begin
        db.ExecuteQuery('SELECT ' +
          Table.Fields[i].ForeignKey.FieldName + ' FROM ' +
          Table.Fields[i].ForeignKey.Table.Name + ' ORDER BY ' +
          Table.Fields[i].ForeignKey.Table.OrderBy);
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
  Table := nil;
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
  Lim := ' NEXT VALUE FOR ' + Table.GenName;
  for i := 0 to High(FComboBoxes) do
    Lim := Lim + ', :param' + IntToStr(i);
  with db.SQLQuery do begin
    Active := False;
    Close;
    SQL.Text := 'INSERT INTO ' + Table.en + ' VALUES(' + Lim + ' );';
  end;

  for i := 0 to High(FComboBoxes) do begin
    Case Table.TableType of
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
  if ACardType = ctAddition then begin
    FEditPanel := TAddPanel.Create(Self);
    FEditPanel.Parent := Self;
  end;
  inherited Show;
end;

end.

