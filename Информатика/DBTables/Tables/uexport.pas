unit UExport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

 THTMLFormat = class
 protected
   Elems: ansistring;
   Rows: ansistring;
   Cols: ansistring;
 public
   Table: ansistring;
   procedure AddElem(s: string);
   procedure AddTableCol();
   procedure AddTableRow();
   procedure AddTable();
   function AddRow(s: ansistring): ansistring;
   procedure ClearColData();
   procedure ClearElemsData();
 end;

implementation

procedure THTMLFormat.AddElem(s: string);
begin
  if s = '' then exit;
  Elems := Elems + '<div style="width:300px">' + s + '</div>';
end;

procedure THTMLFormat.AddTableCol();
begin
  Cols := Cols + #10#13 + '<td>' + Elems + '</td>';
end;

procedure THTMLFormat.AddTableRow();
begin
  Rows := Rows + #10#13 + '<tr>' + Cols + '</tr>'
end;

procedure THTMLFormat.AddTable();
begin
  Table := Table +
    '<head><style>table,th,td{border:1px solid black;}</style></head><table>' +
    Rows + '</table>';
end;

function THTMLFormat.AddRow(s: ansistring): ansistring;
begin
  Result := s  + '</br>';
end;

procedure THTMLFormat.ClearColData();
begin
  Cols := '';
end;

procedure THTMLFormat.ClearElemsData();
begin
  Elems := '';
end;

end.

