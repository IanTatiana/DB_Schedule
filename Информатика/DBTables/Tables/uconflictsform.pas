unit UConflictsForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  UTables;

type
  { TConflictForm }

  TConflictForm = class(TForm)
    ConflictTree: TTreeView;
    procedure ConflictTreeGetSelectedIndex(Sender: TObject; Node: TTreeNode);
    procedure ConflictTreeSelectionChanged(Sender: TObject);
  private
    ConflictTable, ConflictListTable: TTable;
  public
    constructor Create(TheOwner: TComponent); override;
  end;

var
  ConflictsForm: TConflictForm;

implementation

{$R *.lfm}

{ TConflictForm }

constructor TConflictForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

procedure TConflictForm.ConflictTreeSelectionChanged(Sender: TObject);
begin
  ShowMessage(IntToStr(ConflictTree.Selected.Index));
end;

procedure TConflictForm.ConflictTreeGetSelectedIndex(Sender: TObject;
  Node: TTreeNode);
begin
  //
end;

end.

