unit UTableShow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, DBGrids,
  UTables, IBConnection, sqldb, db, Menus, ExtCtrls, StdCtrls, DbCtrls,
  ComCtrls, UFilter;

type

  { TForm2 }

  TForm2 = class(TForm)
  protected
    FTableIndex: integer;
  published
    ShowBtn: TButton;
    DBGrid1: TDBGrid;
    FilterPanel: TPanel;
    AddFilt: TMngFiltButton;
    property TableIndex: integer read FTableIndex write FTableIndex;
    procedure Show(ATableIdx: integer); overload;
    procedure ShowBtnClick(Sender: TObject);
    constructor Create(TheOwner: TComponent); override;
  end;

var
  Form2: TForm2;

implementation

uses main;

{$R *.lfm}

{ TForm2 }
{
procedure TForm2.FormCreate(Sender: TObject);
begin
 { AddFilt := TMngFiltButton.Create(FilterPanel);
  with AddFilt do begin
    Top := 72;
    Left := 16;
    Width := 128;
    Height := 28;
    Parent := FilterPanel;
    Caption := ' Добавить фильтр ';
  end;      }
 // SetLength(Filt, Length(Filt) + 1);
 // Filt[High(Filt)] := TFilter.Create(FilterPanel, Tables[0], 0);
end;   }
constructor TForm2.Create(TheOwner: TComponent);
begin

end;

procedure TForm2.Show(ATableIdx: integer);
begin
  //Filt[High(Filt)] := TFilter.Create(FilterPanel, Tables[ATableIdx], 0);
  FTableIndex := ATableIdx;
  Show;
end;

procedure TForm2.ShowBtnClick(Sender: TObject);
var
  i: integer;
  res, list, q: string;
begin
  q := 'SELECT'; list := ' ';
 { res := ' a.' +
    Filt[0].FiltTable.Fields[Filt[0].Field.ItemIndex + 1].Name +
    Filt[0].LimitsArray[1].CmpSign.Text +
    Filt[0].LimitsArray[1].Limit.Text;  }
{  for i := 1 to High(Filt) do begin
    list := list +  ' ' + Filt[0].Field.Items[i];
    res := res + 'AND a.' +
      Filt[0].FiltTable.Fields[Filt[i].Field.ItemIndex + 1].Name +
      Filt[i].LimitsArray[1].CmpSign.Text +
      Filt[0].LimitsArray[1].Limit.Text;
  end;   }

 // Form1.ExecuteQuery(FTableIndex, q);
end;

end.
