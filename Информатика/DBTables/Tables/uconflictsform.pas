unit UConflictsForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, StdCtrls, UTables, UTableForm, UMyDBTools;

type

  { TConflictForm }

  TConflictForm = class(TForm)
    ConflictTree: TTreeView;
    ShowBtn: TButton;
    Panel: TPanel;
    procedure ShowBtnClick(Sender: TObject);
  public
    constructor Create(TheOwner: TComponent); override;
  end;

const
  TableNum = 9;

var
  ConflictsForm: TConflictForm;

implementation

{$R *.lfm}

{ TConflictForm }

procedure TConflictForm.ShowBtnClick(Sender: TObject);
var
  Table: TTableForm;
begin
  Table := TTableForm.CreateNew(Self, TableNum);
  Table.Show;
end;

constructor TConflictForm.Create(TheOwner: TComponent);
var
  db: TMyDBTools;
  k: string;
begin
  inherited Create(TheOwner);
  db := TMyDBTools.Create(Self, TableNum);
  db.ExecuteQuery('SELECT ' +
    Confl_ListTable.SelectFields[2].OrderID + ', ' +
    Confl_ListTable.FieldsList() +
    ' FROM ' + Confl_ListTable.JoinName +
    ' ORDER BY ' + Confl_ListTable.SelectFields[2].OrderID);
  k := db.SQLQuery.Fields[3].AsString;
  while not db.SQLQuery.EOF do begin
    ConflictTree.Items.AddChild(nil, db.SQLQuery.Fields[3].AsString);
    while (k = db.SQLQuery.Fields[3].AsString) and (not db.SQLQuery.EOF) do begin
      ConflictTree.Items.AddChild(
        ConflictTree.Items.FindNodeWithText(db.SQLQuery.Fields[3].AsString),
        db.SQLQuery.Fields[2].AsString);
      db.SQLQuery.Next;
    end;
    k := db.SQLQuery.Fields[3].AsString;
  end;
end;

end.

