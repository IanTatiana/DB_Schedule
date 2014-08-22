unit UFilter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, ExtCtrls, UTables, Buttons, Dialogs, Controls,
  UMyDBTools, Forms, UPeriods;

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
    procedure FilterChangeVal(Sender: TObject);
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
    function GetFilterVal(i: integer): string;
    function GetFilterCaption(i: integer): string;
    function GetCmpSign(i: integer): string;
    function GetFilterCount(): integer;
  end;

var
  FilterActive: boolean;

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
  FilterActive := False;
  SetLength(Filters, Length(Filters) + 1);
  Filters[High(Filters)] := TFilter.Create(
    Self, Filters[0].TableTag, Filters[High(Filters) - 1].MaxTop + 16,
    Filters[High(Filters) - 1].LLeft);
  RefreshBtn();
end;

procedure TFilterPanel.DelBtnClick(Sender: TObject);
begin
  FilterActive := False;
  if Length(Filters) = 1 then Exit;
  Filters[High(Filters)].Free;
  SetLength(Filters, Length(Filters) - 1);
  RefreshBtn();
end;

procedure TFilterPanel.ShowBtnClick(Sender: Tobject);
var
  i: integer;
  cmp, lim: string;
begin
  FilterActive := True;
  if Parent is TScheduleTable then
    (Parent as TScheduleTable).UpdateScheduleGrid();
  DBTools.SQLQuery.Active := False;
  DBTools.SQLQuery.Prepare;
  DBTools.SQLQuery.SQL.Text := SelectText + ' WHERE 1 = 1 ';
  for i := 0 to High(Filters) do begin
    if (not (Filters[i].FieldCmbBox.Text = '')) and
      (not (Filters[i].CmpCmbBox.Text = '')) and
      (not (Filters[i].LimEdit.Text = '')) then
    begin
      if Filters[i].CmpCmbBox.Text = 'с' then begin
        cmp := ' like ';
        lim := '%'+Filters[i].LimEdit.Text+'%';
      end
      else begin
        cmp := Filters[i].CmpCmbBox.Text;
        lim := Filters[i].LimEdit.Text;
      end;
      DBTools.SQLQuery.SQL.Text := DBTools.SQLQuery.SQL.Text + ' AND ' +
        Table.SelectFields[Filters[i].FieldCmbBox.ItemIndex + 1].en +
        cmp + ' :param' + IntToStr(i);
      DBTools.SQLQuery.Params.ParamByName('param' + IntToStr(i)).AsString :=
        lim;
    end;
  end;
  //if
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

function TFilterPanel.GetFilterVal(i: integer): string;
begin
  Result := Filters[i].LimEdit.Text;
end;

function TFilterPanel.GetCmpSign(i: integer): string;
begin
  Result := Filters[i].CmpCmbBox.Text;
end;

function TFilterPanel.GetFilterCaption(i: integer): string;
begin
  Result := Filters[i].FieldCmbBox.Text;
end;

function TFilterPanel.GetFilterCount(): integer;
begin
  Result := Length(Filters);
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
    OnChange := @FilterChangeVal;
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
    Items.Add('>=');
    Items.Add('<=');
    Items.Add('с');
    ItemIndex := 0;
    OnChange := @FilterChangeVal;
  end;

  LimEdit := TEdit.Create(APanel);
  with LimEdit do begin
    Parent := APanel;
    Top := CmpCmbBox.Top;
    Left := CmpCmbBox.Left + CmpCmbBox.Width + 4;
    Width := 204;
    OnChange := @FilterChangeVal;
  end;

  MaxTopVal := CmpCmbBox.Top + CmpCmbBox.Height;
  MaxLeftVal := LocalLeft;
end;

procedure TFilter.FilterChangeVal(Sender: TObject);
begin
  FilterActive := False;
end;

destructor TFilter.Destroy;
begin
  FieldCmbBox.Free;
  CmpCmbBox.Free;
  LimEdit.Free;
  inherited;
end;

end.

