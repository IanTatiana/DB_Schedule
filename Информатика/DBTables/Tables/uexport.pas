unit UExport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, comobj;

type

 THTMLFormat = class
 protected
   FHead, FElems, FRows, FCols, FTable: ansistring;
 public
   property Head: ansistring read FHead;
   property Elems: ansistring read FElems;
   property Rows: ansistring read FRows;
   property Cols: ansistring read FCols;
   property Table: ansistring read FTable;
   procedure CreateHeader(s: string);
   procedure AddElem(s: string; headelem: boolean);
   procedure AddTableCol(HeadElem: boolean);
   procedure AddTableRow();
   procedure AddTable();
   function AddRow(s: ansistring): ansistring;
   procedure ClearColData();
   procedure ClearElemsData();
   function HSize(i: integer; s: string): string;
   function MakeTextStrong(s: string): string;
   function MakeHTML(s: ansistring): ansistring;
 end;

implementation

procedure THTMLFormat.CreateHeader(s: string);
begin
  FHead := '<head><style>'#10#13 +
    'table,th,td{'#10#13 +
    'border:1px groove black;'#10#13 +
    'border-collapse:collapse;}'#10#13 +
    '.DivElemSchedule{width: 300px;padding: 12px;}'#10#13 +
    '.DivHeadElemSchedule{width: 300px;padding: 12px;}'#10#13 +
    '.DivHead{width:100%;padding: 48px;}</style>'#10#13 +
    '<div class = "divHead">'+ s +'</div></head>'
end;

procedure THTMLFormat.AddElem(s: string; headelem: boolean);
var
  param: string;
begin
  if s = '' then exit;
  if headelem then
    param := 'DivHeadElemSchedule'
  else
    param := 'DivElemSchedule';
  FElems := FElems + '<div class="' + param + '">' + s + '</div>';
end;

procedure THTMLFormat.AddTableCol(HeadElem: boolean);
var
  param: string;
begin
  param := '';
  if HeadElem then
    param := 'style="background:lightgray"';
  FCols := FCols + #10#13 + '<td valign="top" ' + param + ' >' + FElems + '</td>';
end;

procedure THTMLFormat.AddTableRow();
begin
  FRows := FRows + #10#13 + '<tr>' + FCols + '</tr>'
end;

procedure THTMLFormat.AddTable();
begin
  FTable := '<table>' + FRows + '</table>';
end;

function THTMLFormat.AddRow(s: ansistring): ansistring;
begin
  Result := s  + '</br>';
end;

procedure THTMLFormat.ClearColData();
begin
  FCols := '';
end;

procedure THTMLFormat.ClearElemsData();
begin
  FElems := '';
end;

function THTMLFormat.HSize(i: integer; s: string): string;
begin
  Result := '<H' + IntToStr(i) + '>' + s + '</H' + IntToStr(i) + '>';
end;

function THTMLFormat.MakeTextStrong(s: string): string;
begin
  Result := '<strong>' + s + '</strong>';
end;

function THTMLFormat.MakeHTML(s: ansistring): ansistring;
begin
  Result := '<!DOCTYPE HTML><HTML>'+ FHead + '<body>' +
    FTable + '</body></HTML>'
end;

end.

