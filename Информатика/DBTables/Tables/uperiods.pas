unit UPeriods;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, StdCtrls;

type

  TDateBoxes = record
    Day, Month, Year: TComboBox;
  end;

  TPeriodsPanel = class(TPanel)
  protected
    FPanelName, FFrom, FTo: TLabel;
    FFromGroupBox, FToGroupBox: TDateBoxes;
  public
    constructor Create(AOwner: TPanel; ALeft: integer);
  end;

implementation

constructor TPeriodsPanel.Create(AOwner: TPanel; ALeft: integer);
const
  MonthNames: array [0..11] of string = (
    'Январь', 'Февраль', 'Март', 'Апрель', 'Май', 'Июнь',
    'Июль', 'Август', 'Сентябрь', 'Октябрь', 'Ноябрь', 'Декабрь');
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

  FFromGroupBox.Day := TComboBox.Create(Self);
  with FFromGroupBox.Day do begin
    Left := 42;
    Top := 38;
    Parent := Self;
    Width := 48;
    for i := 1 to 31 do
      Items.Add(IntToStr(i));
    ItemIndex := 0;
  end;

  FFromGroupBox.Month := TComboBox.Create(Self);
  with FFromGroupBox.Month do begin
    Left := 8 + FFromGroupBox.Day.Width + FFromGroupBox.Day.Left;
    Top := 38;
    Width := 84;
    Parent := Self;
    for i := 0 to High(MonthNames) do
        Items.Add(MonthNames[i]);
    ItemIndex := 0;
  end;

  FFromGroupBox.Year := TComboBox.Create(Self);
  with FFromGroupBox.Year do begin
    Left := 8 + FFromGroupBox.Month.Width + FFromGroupBox.Month.Left;
    Top := 38;
    Width := 84;
    Parent := Self;
    for i := 2020 downto 1990 do
      Items.Add(IntToStr(i));
    ItemIndex := 6;
  end;

  FToGroupBox.Day := TComboBox.Create(Self);
  with FToGroupBox.Day do begin
    Left := 42;
    Top := 68;
    Parent := Self;
    Width := 48;
    for i := 1 to 31 do
      Items.Add(IntToStr(i));
    ItemIndex := 0;
  end;

  FToGroupBox.Month := TComboBox.Create(Self);
  with FToGroupBox.Month do begin
    Left := 8 + FToGroupBox.Day.Width + FToGroupBox.Day.Left;
    Top := 68;
    Width := 84;
    Parent := Self;
    for i := 0 to High(MonthNames) do
        Items.Add(MonthNames[i]);
    ItemIndex := 0;
  end;

  FToGroupBox.Year := TComboBox.Create(Self);
  with FToGroupBox.Year do begin
    Left := 8 + FToGroupBox.Month.Width + FToGroupBox.Month.Left;
    Top := 68;
    Width := 84;
    Parent := Self;
    for i := 2020 downto 1990 do
      Items.Add(IntToStr(i));
    ItemIndex := 6;
  end;
end;

end.

