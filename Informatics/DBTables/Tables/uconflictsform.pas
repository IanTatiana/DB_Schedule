unit UConflictsForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, StdCtrls, UTables, UTableForm, UMyDBTools;

type

  { TConflictForm }

  TConflictForm = class(TForm)
    ShowBtn: TButton;
    ConflictTree: TTreeView;
    Panel1: TPanel;
    procedure ShowBtnClick(Sender: TObject);
  public
    FieldOrder: array of string;
    constructor Create(TheOwner: TComponent); override;
    procedure ShowTree(Sender: TObject);
  end;

const
  TableNum = 9;

var
  ConflictsForm: TConflictForm;

implementation

{$R *.lfm}

{ TConflictForm }

function FirstNum(var s: string): integer;
var
  r: string;
begin
  if Length(s) < 1 then begin
    Result := -1;
    Exit;
  end;
  Result := StrToInt(Copy(s, 1, 1));
  Delete(s, 1, 2);
end;

function FirstFieldValue(var s: string): string;
begin
  if Length(s) < 1 then begin
    Result := '';
    Exit;
  end;
  Result := Copy(s, 1, Pos(';', s) - 1);
  Delete(s, 1, Pos(';', s) + 1);
end;

procedure TConflictForm.ShowBtnClick(Sender: TObject);
var
  Table: TTableForm;
  k: integer;
  s, r: string;
begin
  if (ConflictTree.Selected = nil) or ConflictTree.Selected.HasChildren then begin
    ShowMessage('Выберите конфликтующие записи');
    Exit;
  end;
  s := ConflictTree.Selected.Text;
  r := FieldOrder[ConflictTree.Selected.Parent.Index];
  Table := TTableForm.CreateNew(Self, TableNum);
  k := 1;
  while k <> -1 do begin
    k := FirstNum(r);
    if k <> -1 then
      Table.FillFiters(k, FirstFieldValue(s));
  end;
  Table.Caption := Table.Caption + ' - ' + ConflictTree.Selected.Parent.Text;
  Table.Show;
end;

constructor TConflictForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  OnShow := @ShowTree;
  SetLength(FieldOrder, 0);
end;

procedure TConflictForm.ShowTree(Sender: TObject);
var
  db, db2: TMyDBTools;
  i: integer;
  res: string;
begin
  db := TMyDBTools.Create(Self, TableNum);
  db2 := TMyDBTools.Create(Self, TableNum);
  db.ExecuteQuery('SELECT * FROM ' + ConflTable.en);
  res := '';
  while not db.SQLQuery.EOF do begin
    ConflictTree.Items.Add(nil, db.SQLQuery.Fields[1].AsString);
    db2.ExecuteQuery(db.SQLQuery.Fields[2].AsString);
    SetLength(FieldOrder, Length(FieldOrder) + 1);
    FieldOrder[High(FieldOrder)] := db.SQLQuery.Fields[3].AsString;
    while not db2.SQLQuery.EOF do begin
      for i := 0 to db2.SQLQuery.Fields.Count - 1 do
        res := res + db2.SQLQuery.Fields[i].AsString + '; ';
      ConflictTree.Items.AddChild(ConflictTree.Items.FindNodeWithText(
        db.SQLQuery.Fields[1].AsString), res);
        res := ''; db2.SQLQuery.Next;
    end;
    db.SQLQuery.Next;
  end;
end;

end.

