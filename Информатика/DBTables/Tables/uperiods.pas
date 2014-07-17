unit UPeriods;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, StdCtrls, UMyDBTools, Dialogs;

type

  TDateBoxes = class
  protected
    Day, Month, Year: TComboBox;
  public
    constructor Create(AOwner: TPanel; ATop, ALeft: integer);
    function GetDate(): string;
  end;

  TPeriodsPanel = class(TPanel)
  protected
    FPanelName, FFrom, FTo: TLabel;
  public
    db: TMyDBTools;
    FFromGroupBox, FToGroupBox: TDateBoxes;
    ScheduleElemsID: array of string; // ID for records, which is in this period
    CheckBox: TCheckBox;
    constructor Create(AOwner: TPanel; ALeft: integer);
    function FindRecord(AFrom, ATo: string): boolean;
    //
    //function FindElemByID(ID: string): string;
    //procedure Selection(Sender: TObject);
  end;

implementation

//----TDateBoxes----------------------------------------------------------------
constructor TDateBoxes.Create(AOwner: TPanel; ATop, ALeft: integer);
const
  MonthNames: array [0..11] of string = (
    'Январь', 'Февраль', 'Март', 'Апрель', 'Май', 'Июнь',
    'Июль', 'Август', 'Сентябрь', 'Октябрь', 'Ноябрь', 'Декабрь');
var
  i: integer;
begin
  Day := TComboBox.Create(AOwner);
  with Day do begin
    Left := ALeft;
    Top := ATop;
    Parent := AOwner;
    Width := 48;
    for i := 1 to 31 do
      Items.Add(IntToStr(i));
    ItemIndex := 0;
  end;

  Month := TComboBox.Create(AOwner);
  with Month do begin
    Left := 8 + Day.Width + Day.Left;
    Top := ATop;
    Width := 84;
    Parent := AOwner;
    for i := 0 to High(MonthNames) do
        Items.Add(MonthNames[i]);
    ItemIndex := 0;
  end;

  Year := TComboBox.Create(AOwner);
  with Year do begin
    Left := 8 + Month.Width + Month.Left;
    Top := ATop;
    Width := 84;
    Parent := AOwner;
    for i := 2020 downto 1990 do
      Items.Add(IntToStr(i));
    ItemIndex := 6;
  end;
end;

function TDateBoxes.GetDate(): string;
var
  m, d: string;
begin
  if Month.ItemIndex + 1 < 10 then
    m := '0' + IntToStr(Month.ItemIndex + 1)
  else
    m := IntToStr(Month.ItemIndex + 1);
  if Day.ItemIndex + 1  < 10 then
    d := '0' + IntToStr(Day.ItemIndex + 1)
  else
    d := Day.Text;
  Result := '''' + d + '.' + m + '.' + Year.Text + '''';
end;

//----TPeriodsPanel-------------------------------------------------------------
constructor TPeriodsPanel.Create(AOwner: TPanel; ALeft: integer);
var
  i: integer;
begin
  inherited Create(AOwner);
  Width := 520;
  Height := AOwner.Height;
  Left := ALeft;
  Parent := AOwner;
  FPanelName := TLabel.Create(Self);
  with FPanelName do begin
    Left := 18;
    Top := 18;
    Parent := Self;
    Caption := 'Период';
  end;

  FFrom := TLabel.Create(Self);
  with FFrom do begin
    Left := 18;
    Top := 42;
    Parent := Self;
    Caption := 'с';
  end;

  FTo := TLabel.Create(Self);
  with FTo do begin
    Left := 18;
    Top := 70;
    Parent := Self;
    Caption := 'до';
  end;

  FFromGroupBox := TDateBoxes.Create(Self, 38, 42);
  FToGroupBox := TDateBoxes.Create(Self,  68, 42);

  CheckBox := TCheckBox.Create(Self);
  with CheckBox do begin
    Left := 18;
    Top := FToGroupBox.Day.Top + 32;
    Parent := Self;
    Checked := False;
    Caption := 'Учитывать периоды';
  end;
  db := TMyDBTools.Create(nil, 9);
end;

function TPeriodsPanel.FindRecord(AFrom, ATo: string): boolean;
var
  FromDate, ToDate: string;
  tdb: TMyDBTools;
  s: String;
const
  Schedule = 'Schedule_Items A';
  PeriodFrom = 'A.PeriodFrom';
  PeriodTo = 'A.PeriodTo';
begin
  Result := false;
  FromDate := FFromGroupBox.GetDate();
  ToDate := FToGroupBox.GetDate();
  AFrom := '''' + AFrom + '''';
  ATo := '''' + ATo + '''';
  s :=
    'SELECT COUNT(*) FROM ' + Schedule + ' WHERE ( date ' +
    AFrom + '< date ' + FromDate + ' AND date ' + ATo + '< date ' + FromDate + ') OR ( date ' +
    AFrom + '> date ' + ToDate + ' AND date ' + ATo + '> date ' + ToDate + ')' ;
  tdb := TMyDBTools.Create(nil, 9);
  tdb.ExecuteQuery(s);
  Result := tdb.SQLQuery.FieldByName('COUNT').AsInteger = 0;
end;

end.

