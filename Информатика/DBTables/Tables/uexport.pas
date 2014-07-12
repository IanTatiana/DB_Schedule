unit UExport;

{$mode objfpc}{$H+}{$R+}

interface

uses
  Classes, SysUtils, comobj, FileUtil, USchedule, UFilter;

type

  TExport = class
  protected
    FElems, FCell: ansistring;
    function Selection(AHorzName, AVertName: string): string;
    function Filters(AFilterPanel: TFilterPanel; AFilterActive: boolean): string;
    procedure FillCells(AScheduleMatrix: THiperArray);
  public
    procedure CreateExportObject(AScheduleMatrix: THiperArray;
      AFilterPanel: TFilterPanel; AHorzName, AVertName: string;
      AFilterActive: boolean; AFName: string); virtual; abstract;
    procedure AddHeadCellData(s: string; i, j: integer); virtual; abstract;
    procedure AddCellData(s: string; i, j: integer); virtual;
    procedure AddElem(s: string); virtual; abstract;
    procedure GenRow(); virtual;
    function NewLine(s1, s2: string): ansistring; virtual; abstract;
  end;

  THTMLExport = class(TExport)
  protected
    FHead, FRows, FTable: ansistring;
    function br_tag(s: ansistring): ansistring;
    function HTML_tag(s: ansistring): ansistring;
    function Body_tag(s: ansistring): ansistring;
    function HSize_tag(i: integer; s: string): string;
    function TextStrong_tag(s: string): string;
  public
    procedure CreateExportObject(AScheduleMatrix: THiperArray;
      AFilterPanel: TFilterPanel; AHorzName, AVertName: string;
      AFilterActive: boolean; AFName: string); override;
    procedure AddHeadCellData(s: string; i, j: integer); override;
    procedure AddCellData(s: string; i, j: integer); override;
    procedure CreateHeader(s: string);
    procedure AddElem(s: string); override;
    procedure GenRow(); override;
    procedure GenTable();
    function NewLine(s1, s2: string): ansistring; override;
  end;

  TExcelExport = class(TExport)
  protected
    ExcelObj, WorkBook, Sheet, VArray, Rng: Variant;
  public
    procedure CreateExportObject(AScheduleMatrix: THiperArray;
      AFiltPanel: TFilterPanel; AHorzName, AVertName: string;
      AFiltActive: boolean; AFName: string); override;
    procedure AddHeadCellData(s: string; i, j: integer); override;
    procedure AddCellData(s: string; i, j: integer); override;
    procedure AddElem(s: string); override;
    function NewLine(s1, s2: string): ansistring; override;
  end;

const
  ColWidth = 30;

implementation

//----TExport-------------------------------------------------------------------
function TExport.Selection(AHorzName, AVertName: string): string;
begin
  Result := 'По горизонтали: ' + AHorzName + '; '#13#10 +
    'по вертикали: ' + AVertName + '; ';
end;

function TExport.Filters(
  AFilterPanel: TFilterPanel; AFilterActive: boolean): string;
var
  fval, fcmp, fcap, t2: string;
  i: integer;
begin
  Result := 'Активные фильтры: ';  t2 := '';
  if AFilterActive then begin
    for i := 0 to AFilterPanel.GetFilterCount() - 1 do begin
      fcap := AFilterPanel.GetFilterCaption(i);
      fval := AFilterPanel.GetFilterVal(i);
      fcmp := AFilterPanel.GetCmpSign(i);
      if (fcap <> '') and (fval <> '') and (fcmp <> '') then
        t2 := t2 + #10#13 + fcap + ' ' + fcmp + ' ' + fval;
    end;
  end;
  if t2 = '' then
    Result := 'Активных фильтров нет'
  else
    Result := Result + #10#13 + t2;
end;

procedure TExport.FillCells(AScheduleMatrix: THiperArray);
var
  i, j, k, r: integer;
  s: string;
begin
  for j := 0 to High(AScheduleMatrix[0]) do begin
    for i := 0 to High(AScheduleMatrix) do begin
      if (i = 0) or (j = 0) then
        AddHeadCellData(AScheduleMatrix[i][j][0].Header, j, i)
      else begin
        r := 0;
        while AScheduleMatrix[i][j][r].SchElemField[0] <> '' do begin
          s := '';
          for k := 0 to High(AScheduleMatrix[i][j][r].SchElemField) do
            s := s + NewLine(SchElemName[k], AScheduleMatrix[i][j][r].SchElemField[k]);
          AddElem(s);
          r += 1;
        end;
        AddCellData(FElems, j, i);
      end;
    end;
    GenRow();
  end;
end;

procedure TExport.GenRow();
begin
  FCell := '';
end;

procedure TExport.AddCellData(s: string; i, j: integer);
begin
  FElems := '';
end;

//----TExcelExport--------------------------------------------------------------
procedure TExcelExport.CreateExportObject(AScheduleMatrix: THiperArray;
  AFiltPanel: TFilterPanel; AHorzName, AVertName: string;
  AFiltActive: boolean; AFName: string);
var
  i, j, k, r: integer;
  s: string;
begin
  ExcelObj := CreateOleObject('Excel.Application');
  WorkBook := ExcelObj.WorkBooks.Add;
  Sheet := WorkBook.ActiveSheet;

  Sheet.Cells[1, 1] :=  WideString(UTF8ToSys('Расписание'));
  Sheet.Cells[1, 1].Font.Bold := True;
  Sheet.Cells[1, 1].Font.Size := 24;

  Sheet.Cells[2, 1] := WideString(UTF8ToSys(Selection(AHorzName, AVertName)));
  Sheet.Cells[2, 1].VerticalAlignment := 1;

  Sheet.Cells[3, 1] := WideString(UTF8ToSys(Filters(AFiltPanel, AFiltActive)));
  Sheet.Cells[3, 1].VerticalAlignment := 1;

  FillCells(AScheduleMatrix);
  Rng := Sheet.Range['A1: B4'];
  Rng.EntireColumn.AutoFit;

  ExcelObj.DisplayAlerts := False;
  ExcelObj.Worksheets[1].SaveAs(WideString(UTF8ToSys(AFName)));
  ExcelObj.Application.Quit;
end;

procedure TExcelExport.AddHeadCellData(s: string; i, j: integer);
begin
  i += 2; j += 2;
  Sheet.Cells[i, j] :=  WideString(UTF8ToSys(s));
  Sheet.Cells[i, j].Font.Bold := True;
  Sheet.Cells[i, j].ColumnWidth := ColWidth;
  Sheet.Cells[i, j].WrapText := True;
  Sheet.Cells[i, j].VerticalAlignment := 1;
  Sheet.Cells[i, j].HorizontalAlignment := 3;
end;

procedure TExcelExport.AddCellData(s: string; i, j: integer);
begin
  i += 2; j += 2;
  Sheet.Cells[i, j] := WideString(UTF8ToSys(s));
  Sheet.Cells[i, j].WrapText := True;
  Sheet.Cells[i, j].VerticalAlignment := 1;
  Sheet.Cells[i, j].HorizontalAlignment := 1;
  inherited;
end;

procedure TExcelExport.AddElem(s: string);
begin
  FElems := FElems + s + #10#13;
end;

function TExcelExport.NewLine(s1, s2: string): ansistring;
begin
  Result := s1 + s2 + '; '#10#13;
end;

//----THTMLExport---------------------------------------------------------------
procedure THTMLExport.CreateExportObject(AScheduleMatrix: THiperArray;
  AFilterPanel: TFilterPanel; AHorzName, AVertName: string;
  AFilterActive: boolean; AFName: string);
var
  f: text;
  i, j, k, r: integer;
begin
  CreateHeader(
    br_tag(HSize_tag(2, 'Расписание')) +
    br_tag(Selection(AHorzName, AVertName)) +
    br_tag(Filters(AFilterPanel, AFilterActive)));
  FillCells(AScheduleMatrix);
  GenTable();

  AssignFile(f, UTF8ToSys(AFName));
  Rewrite(f);
  Write(f, HTML_tag(FTable));
  CloseFile(f);
end;

procedure THTMLExport.CreateHeader(s: string);
begin
  FHead := #10#13 +
    '  <head>'#10#13 +
    '    <style>'#10#13 +
    '      table,th,td{'#10#13 +
    '        border:1px groove black;'#10#13 +
    '        border-collapse:collapse;}'#10#13 +
    '      .tdHead{background: #CCC}'#10#13 +
    '      .divElemSchedule{width: 300px;padding: 12px;}'#10#13 +
    '      .divHeadElemSchedule{width: 300px;padding: 12px;}'#10#13 +
    '      .divHead{width:100%;padding: 48px;}'#10#13 +
    '    </style>'#10#13 +
    '    <div class = "divHead">'#10#13'  ' + s +
    '    </div>'#10#13 +
    '  </head>'
end;

procedure THTMLExport.AddElem(s: ansistring);
begin
  FElems := FElems +
    #10#13'      <div class="divElemSchedule">' + s + #10#13'      </div>';
end;

procedure THTMLExport.AddHeadCellData(s: string; i, j: integer);
begin
  FCell := FCell +
    #10#13'    <td class="tdHead"><div class="divHeadElemSchedule">' +
    TextStrong_tag(s) + #10#13'    </div></td>';
end;

procedure THTMLExport.AddCellData(s: string; i, j: integer);
begin
  FCell := FCell + #10#13'    <td valign="top">' + s + #10#13'    </td>';
  inherited;
end;

procedure THTMLExport.GenRow();
begin
  FRows := FRows + #10#13'  <tr>' + FCell + #10#13'  </tr>';
  inherited;
end;

procedure THTMLExport.GenTable();
begin
  FTable := #10#13'  <table>' + FRows + #10#13'  </table>';
end;

function THTMLExport.NewLine(s1, s2: string): ansistring;
begin
  Result := br_tag(TextStrong_tag(s1) + s2);
end;

function THTMLExport.br_tag(s: ansistring): ansistring;
begin
  Result := s  + '</br>';
end;

function THTMLExport.HSize_tag(i: integer; s: string): string;
begin
  Result := '<H' + IntToStr(i) + '>' + s + '</H' + IntToStr(i) + '>';
end;

function THTMLExport.TextStrong_tag(s: string): string;
begin
  Result := #10#13'  <strong>' + s + '  </strong>';
end;

function THTMLExport.Body_tag(s: ansistring): ansistring;
begin
  Result := #10#13'  <body>' + s + #10#13'  </body>';
end;

function THTMLExport.HTML_tag(s: ansistring): ansistring;
begin
  Result := '<!DOCTYPE HTML><HTML>' + FHead + s + #10#13'</HTML>'
end;

end.

