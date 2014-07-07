unit UExport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

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
   procedure AddElem(s: string);
   procedure AddTableCol();
   procedure AddTableRow();
   procedure AddTable();
   function AddRow(s: ansistring): ansistring;
   procedure ClearColData();
   procedure ClearElemsData();
 end;

implementation

procedure THTMLFormat.CreateHeader(s: string);
begin
  FHead := '<head><div class = "divHead">'+ s +'</div></head>'
end;

procedure THTMLFormat.AddElem(s: string);
begin
  if s = '' then exit;
  FElems := FElems + '<div class = "divElemSchedule">' + s + '</div>';
end;

procedure THTMLFormat.AddTableCol();
begin
  FCols := FCols + #10#13 + '<td>' + FElems + '</td>';
end;

procedure THTMLFormat.AddTableRow();
begin
  FRows := FRows + #10#13 + '<tr>' + FCols + '</tr>'
end;

procedure THTMLFormat.AddTable();
begin
  FTable := FTable +
    '<head><style>table,th,td{border:1px groove black;}'#10#13 +
    '.divElemSchedule{width: 300px; padding: 12px;}'#10#13 +
    '.divHead{width: 100%; padding: 48px;}'#10#13'</style></head><table>' +
    FRows + '</table>';
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

end.

