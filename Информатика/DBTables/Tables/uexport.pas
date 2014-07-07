unit UExport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

 THTMLFormat = class
 protected
   Rows: ansistring;
   Cols: ansistring;
 public
   Table: ansistring;
   procedure AddTableCol(s: string);
   procedure AddTableRow();
   procedure AddTable();
   function AddRow(s: ansistring): ansistring;
   procedure ClearData();
 end;

implementation

procedure THTMLFormat.AddTableCol(s: string);
begin
  Cols := Cols + #10#13 + '<td style="width:300px">' + s + '</td>';
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

procedure THTMLFormat.ClearData();
begin
  Cols := '';
end;

end.

