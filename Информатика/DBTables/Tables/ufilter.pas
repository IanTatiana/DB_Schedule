unit UFilter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, ExtCtrls, UTables, Buttons, Dialogs, Controls,
  UMyDBTools, Forms;

const
  FilterWidth = 256;

type

  TFilter = class
  protected
    LocalTop,
    LocalLeft: integer;
    Num: integer;
    MaxTopVal,
    MaxLeftVal: integer;
  public
    FieldCmbBox: TComboBox;
    CmpCmbBox: TComboBox;
    LimEdit: TEdit;
    property LTop: integer read LocalTop write LocalTop;
    property LLeft: integer read LocalLeft write LocalLeft;
    property MaxTop: integer read MaxTopVal;
    property MaxLeft: integer read MaxLeftVal;
    property TableTag: integer read Num;
    constructor Create(APanel: TPanel; ATableTag, ALTop, ALLeft: integer);
    destructor Destroy; override;
  end;

  TFilterPanel = class(TPanel)
  protected
    FiltLab: TLabel;
    Filters: array of TFilter;
    DelBtn, AddBtn, ShowBtn: TButton;

    Table: TTable;
    DBTools: TMyDBTools;
  public
    SelectText,
    OrderByText: string;
    procedure AddBtnClick(Sender: TObject);
    procedure DelBtnClick(Sender: TObject);
    procedure ShowBtnClick(Sender: TObject);
    procedure RefreshBtn();
    constructor Create(AOwner: TForm; ADBTools: TMyDBTools; Num: Integer = 0);
    procedure AddAndFillFilter(i: integer; s: string);
  end;

implementation

uses
  UTableForm, USchedule;

//----TFilterPanel--------------------------------------------------------------
constructor TFilterPanel.Create(
  AOwner: TForm; ADBTools: TMyDBTools; Num: Integer = 0 );
begin
  inherited Create(AOwner);
  Parent := AOwner;
  Align := alLeft;
  Width := 320;
  DBTools := ADBTools;
  Table := Tables[Num];
  SelectText := 'SELECT ' + Table.FieldsList() + ' FROM ' + Table.JoinName;
  OrderByText := '';
  FiltLab := TLabel.Create(Self);
  with FiltLab do begin
    Parent := Self;
    Top := 24;
    Left := 24;
    Text := 'Фильтр';
  end;

  SetLength(Filters, Length(Filters) + 1);
  Filters[0] := TFilter.Create(
    Self, Num, FiltLab.Top + FiltLab.Height + 16, FiltLab.Left);

  AddBtn := TButton.Create(Self);
  with AddBtn do begin
    Parent := Self;
    Top := Filters[High(Filters)].MaxTop + 16;
    Left := Filters[High(Filters)].MaxLeft;
    Caption := 'Добавить';
    Width := 80;
    Height := 32;
    OnClick := @AddBtnClick;
  end;
  DelBtn := TButton.Create(Self);
  with DelBtn do begin
    Parent := Self;
    Top := Filters[High(Filters)].MaxTop + 16;
    Left := AddBtn.Left + AddBtn.Width + 4;
    Caption := 'Удалить';
    Width := 80;
    Height := 32;
    OnClick := @DelBtnClick;
  end;
  ShowBtn := TButton.Create(Self);
  with ShowBtn do begin
    Parent := Self;
    Top := Filters[High(Filters)].MaxTop + 16;
    Left := DelBtn.Left + DelBtn.Width + 4;
    Caption := 'Показать';
    Width := 88;
    Height := 32;
    OnClick := @ShowBtnClick;
  end;
end;

procedure TFilterPanel.AddBtnClick(Sender: TObject);
begin
  SetLength(Filters, Length(Filters) + 1);
  Filters[High(Filters)] := TFilter.Create(
    Self, Filters[0].TableTag, Filters[High(Filters) - 1].MaxTop + 16,
    Filters[High(Filters) - 1].LLeft);
  RefreshBtn();
end;

procedure TFilterPanel.DelBtnClick(Sender: TObject);
begin
  if Length(Filters) = 1 then Exit;
  Filters[High(Filters)].Free;
  SetLength(Filters, Length(Filters) - 1);
  RefreshBtn();
end;

procedure TFilterPanel.ShowBtnClick(Sender: Tobject);
var
  i: integer;
  Limits: array of string;
begin
  if Parent is TScheduleTable then
    (Parent as TScheduleTable).UpdateScheduleGrid();
  DBTools.SQLQuery.Active := False;
  DBTools.SQLQuery.Prepare;
  SetLength(Limits, 0);
  for i := 0 to High(Filters) do begin
    if (not (Filters[i].FieldCmbBox.Text = '')) and
      (not (Filters[i].CmpCmbBox.Text = '')) and
      (not (Filters[i].LimEdit.Text = '')) then
    begin
      SetLength(Limits, Length(Limits) + 1);
      Limits[High(Limits)] :=
        Table.SelectFields[Filters[i].FieldCmbBox.ItemIndex + 1].en +
        Filters[i].CmpCmbBox.Text;
    end;
  end;
  DBTools.SQLQuery.SQL.Text := SelectText + ' WHERE 1=1 ';
  for i := 0 to High(Limits) do begin
    DBTools.SQLQuery.SQL.Text :=
      DBTools.SQLQuery.SQL.Text + ' AND ' + Limits[i] + ' :param' + IntToStr(i);
    DBTools.SQLQuery.Params.ParamByName('param' + IntToStr(i)).AsString :=
      Filters[i].LimEdit.Text;
  end;
  DBTools.ExecuteQuery(DBTools.SQLQuery.SQL.Text + ' ' + OrderByText);
  if Parent is TTableForm then
    (Parent as TTableForm).Russification;
  if Parent is TScheduleTable then
    (Parent as TScheduleTable).FillScheduleMatrix();
end;

procedure TFilterPanel.RefreshBtn();
begin
  AddBtn.Top := Filters[High(Filters)].MaxTop + 16;
  DelBtn.Top := AddBtn.Top;
  ShowBtn.Top := AddBtn.Top;
end;

procedure TFilterPanel.AddAndFillFilter(i: integer; s: string);
begin
  Filters[High(Filters)].FieldCmbBox.ItemIndex := i;
  Filters[High(Filters)].LimEdit.Text := s;
  AddBtnClick(AddBtn);
end;

//----TFilter-------------------------------------------------------------------
constructor TFilter.Create(APanel: TPanel; ATableTag, ALTop, ALLeft: integer);
var
  i: integer;
begin
  LocalTop := ALTop;
  LocalLeft := ALLeft;
  Num := ATableTag;
  FieldCmbBox := TComboBox.Create(APanel);
  with FieldCmbBox do begin
    Style := csDropDownList;
    Parent := APanel;
    Top := LocalTop;
    Left := LocalLeft;
    Width :=  FilterWidth;
    for i := 1 to High(Tables[ATableTag].SelectFields) do
      Items.Add(Tables[ATableTag].SelectFields[i].ru);
    ItemIndex := 0;
  end;

  CmpCmbBox := TComboBox.Create(APanel);
  with CmpCmbBox do begin
    Style := csDropDownList;
    Parent := APanel;
    Top := LocalTop + FieldCmbBox.Height + 4;
    Left := LocalLeft;
    Width := 48;
    Items.Add('=');
    Items.Add('>');
    Items.Add('<');
    ItemIndex := 0;
  end;

  LimEdit := TEdit.Create(APanel);
  with LimEdit do begin
    Parent := APanel;
    Top := CmpCmbBox.Top;
    Left := CmpCmbBox.Left + CmpCmbBox.Width + 4;
    Width := 204;
  end;

  MaxTopVal := CmpCmbBox.Top + CmpCmbBox.Height;
  MaxLeftVal := LocalLeft;
end;

destructor TFilter.Destroy;
begin
  FieldCmbBox.Free;
  CmpCmbBox.Free;
  LimEdit.Free;
  inherited;
end;

end.

