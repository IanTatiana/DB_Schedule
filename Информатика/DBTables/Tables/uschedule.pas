unit USchedule;

{$mode objfpc}{$H+}

interface

uses
  types, Classes, SysUtils, Forms, Grids, ExtCtrls, UTables, SQLdb,
  IBConnection, db, Dialogs, UMyDBTools, StdCtrls, Graphics, Controls,
  UFilter, UEditCardForm;

type
  TScheduleElem =  record
    Header, ID: string;
    SchElemField: array [0..7] of string;
  end;

  TScheduleTable = class(TForm)
  protected
    Table: TTable;
    FilterPanel: TFilterPanel;
    ToolsPanel,
    SectionPanel,
    CheckPanel,
    PreferPanel: TPanel;

    L_Horizontal,
    L_Vertical: TLabel;
    CB_Horz,
    CB_Vert: TComboBox;
    ShowBtn: TButton;

    ScheduleGrid: TDrawGrid;
    ScheduleElem: TScheduleElem;
    ShowHeads,
    ShowAll: boolean;
    CheckIndicate: Array [0..7] of TCheckBox;
    Indicate: TImage;
    DG_DblClick: boolean;
    OrderInCell: integer;
    CurPos: TPoint;
    AddBtn,
    DelBtn,
    UpdateBtn: TButton;
    procedure ChangeIndicate(Sender: TObject);
    procedure CheckClick(Sender: TObject);
    procedure PreferClick(Sender: TObject);
    procedure ShowAllClick(Sender: TObject);
    procedure CreateSectionPanel();
    procedure CreateCheckPanel();
    procedure CreatePreferPanel();
    procedure SetCellHeight(aRow, r: integer);
    procedure DrawGridDblClick(Sender: TObject);
    procedure DrawGridDrawCell(Sender: TObject;
      aCol, aRow: Integer; aRect: TRect; aState:TGridDrawState);
    procedure DrawGridMouseMove(Sender: TObject;Shift: TShiftState; X, Y: Integer);

    procedure CreateEditBtn(aRect: TRect; CursorPos: TPoint);
    procedure FreeEditBtn();
    procedure InitSelectedData();
    procedure AddElem(Sender: TObject);
    procedure DelElem(Sender: TObject);
    procedure UpdateElem(Sender: TObject);
  public
    ScheduleMatrix: array of array of array of TScheduleElem;
    ShowElemOfCell: array of array of boolean;
    DBTools: TMyDBTools;
    constructor CreateNew(AOwner: TComponent; Num: Integer=0); override;
    function GetCountFrom(idx: integer): integer;
    function GetFieldName(AFieldName: string): string;
    procedure ShowSchedule(Sender: TObject);
    procedure UpdateScheduleGrid();
    procedure FillScheduleMatrix();
    procedure Show;
  end;

const
  CWidth_h = 380;
  RHeight_h = 48;
  CWidth = 380;
  RHeight = 110;
var
  ScheduleTable: TScheduleTable;

implementation

//----TScheduleTable------------------------------------------------------------
constructor TScheduleTable.CreateNew(AOwner: TComponent; Num: Integer=0);
begin
  inherited CreateNew(AOwner);
  Width := 1148;
  Height := 612;
  Position := poScreenCenter;
  Caption := 'Расписание';
  Table := Tables[9];
  DBTools := TMyDBTools.Create(AOwner, Num);

  ToolsPanel := TPanel.Create(Self);
  with ToolsPanel do begin
    Height := 128;
    Align := Align.alTop;
    Parent := Self;
  end;
  FilterPanel := TFilterPanel.Create(Self, DBTools, 9);
  CreateSectionPanel();
  CreateCheckPanel();
  CreatePreferPanel();
  DG_DblClick := False;
  ScheduleGrid := TDrawGrid.Create(Self);
  with ScheduleGrid do begin
    Align := Align.alClient;
    Parent := Self;
    OnDrawCell := @DrawGridDrawCell;
    OnDblClick := @DrawGridDblClick;
    OnMouseMove:= @DrawGridMouseMove;
  end;
  EditCardForm.Free; EditCardForm := nil;
  EditCardForm := TEditCard.CreateNew(Self, Num);
  ShowSchedule(Self);
end;

procedure TScheduleTable.Show;
begin
  ShowSchedule(Self);
  inherited Show;
end;

procedure TScheduleTable.DrawGridDblClick(Sender: TObject);
var
  i, Col, Row: integer;
begin
  Col := ScheduleGrid.Col;
  Row := ScheduleGrid.Row;
  ShowElemOfCell[Col][Row] :=
    not ShowElemOfCell[ScheduleGrid.Col][ScheduleGrid.Row];
  for i := 0 to High(ShowElemOfCell) do begin
    if (Length(ScheduleMatrix[i][Row]) <= Length(ScheduleMatrix[Col][Row])) then
      ShowElemOfCell[i][Row] :=
        ShowElemOfCell[ScheduleGrid.Col][ScheduleGrid.Row]
    else
      ShowElemOfCell[i][Row] := False;
  end;
  if ShowElemOfCell[Col][Row] then
    ScheduleGrid.RowHeights[Row] := High(ScheduleMatrix[Col][Row]) * RHeight
  else
    ScheduleGrid.RowHeights[Row] := RHeight;
end;

procedure TScheduleTable.DrawGridMouseMove(
  Sender: TObject; Shift: TShiftState; X, Y: integer);
var
  Col, Row: integer;
begin
  CurPos := Point(X, Y);
  ScheduleGrid.MouseToCell(X, Y, Col, Row);
  if (Col = 0) or (Row = 0) then begin
    Exit;
  end;
  FreeEditBtn();
  CreateEditBtn(ScheduleGrid.CellRect(Col, Row), Point(X, Y));
end;

procedure TScheduleTable.CreateEditBtn(aRect: TRect; CursorPos: TPoint);
var
  Col, Row: integer;
const
  side = 24;
begin
  OrderInCell := (CursorPos.Y - aRect.Top) div RHeight;
  ScheduleGrid.MouseToCell(CursorPos.X, CursorPos.Y, Col, Row);
  AddBtn := TButton.Create(ScheduleGrid);
  with AddBtn do begin
    Parent := ScheduleGrid;
    Height := side;
    Width := side;
    if Length(ScheduleMatrix[Col][Row]) > 1 then
      Top := aRect.Top + ((CursorPos.Y - aRect.Top) div RHeight) * RHeight
    else
      Top := aRect.Top;
    Left := aRect.Right - Width;
    Caption := ' + ';
    OnClick := @AddElem;
  end;
  DelBtn := TButton.Create(ScheduleGrid);
  with DelBtn do begin
    Parent := ScheduleGrid;
    Height := side;
    Width := side;
    Top := AddBtn.Top + side;
    Left := AddBtn.Left;
    Caption := ' - ';
    OnClick := @DelElem;
  end;
  UpdateBtn := TButton.Create(ScheduleGrid);
  with UpdateBtn do begin
    Parent := ScheduleGrid;
    Height := side;
    Width := side;
    Top := DelBtn.Top + side;
    Left := AddBtn.Left;
    Caption := ' ! ';
    OnClick := @UpdateElem;
  end;
end;

procedure TScheduleTable.AddElem(Sender: TObject);
begin
  CallingForm := Self as TScheduleTable;
  CallingForm.OnShow := @ShowSchedule;
  EditCardForm.Show(9, ctAddition);
end;

procedure TScheduleTable.InitSelectedData();
var
  Col, Row, i: integer;
begin
  ScheduleGrid.MouseToCell(CurPos.X, CurPos.Y, Col, Row);
  ID := ScheduleMatrix[Col][Row][OrderInCell].ID;
  SetLength(SelSchElem, 0);
  for i := 0 to High(ScheduleMatrix[Col][Row][OrderInCell].SchElemField) do
  begin
    SetLength(SelSchElem, Length(SelSchElem) + 1);
    SelSchElem[High(SelSchElem)] :=
      ScheduleMatrix[Col][Row][OrderInCell].SchElemField[i];
  end;
end;

procedure TScheduleTable.DelElem(Sender: TObject);
begin
  InitSelectedData();
  CallingForm := Self as TScheduleTable;
  EditCardForm.Show(9, ctDeletion);
end;

procedure TScheduleTable.UpdateElem(Sender: TObject);
begin
  InitSelectedData();
  CallingForm := Self as TScheduleTable;
  EditCardForm.Show(9, ctUpdating);
end;

procedure TScheduleTable.FreeEditBtn();
begin
  AddBtn.Free;
  DelBtn.Free;
  UpdateBtn.Free;
end;

procedure TScheduleTable.CreateSectionPanel();
var
  i: integer;
const
  indent = 24;
begin
  SectionPanel := TPanel.Create(ToolsPanel);
  with SectionPanel do begin
    Height := ToolsPanel.Height;
    Width := 260;
    Parent := ToolsPanel;
  end;

  L_Horizontal := TLabel.Create(SectionPanel);
  with L_Horizontal do begin
    Parent := SectionPanel;
    Top := 28;
    Left := Top;
    Caption := 'X :';
  end;
  CB_Horz := TComboBox.Create(SectionPanel);
  with CB_Horz do begin
    Style := csDropDownList;
    Parent := SectionPanel;
    Top := indent;
    Left := L_Horizontal.Left + indent;
    Width := 160;
    for i := 1 to High(Table.Fields) do
      Items.Add(Table.SelectFields[i].ru);
    ItemIndex := 5;
    OnSelect := @ChangeIndicate;
  end;

  L_Vertical := TLabel.Create(SectionPanel);
  with L_Vertical do begin
    Parent := SectionPanel;
    Top := L_Horizontal.Top + indent;
    Left := L_Horizontal.Left;
    Caption := 'Y :';
  end;
  CB_Vert := TComboBox.Create(SectionPanel);
  with CB_Vert do begin
    Style := csDropDownList;
    Parent := SectionPanel;
    Top := L_Horizontal.Top + indent;
    Left := CB_Horz.Left;
    Width := 160;
    for i := 1 to High(Table.Fields) do
      Items.Add(Table.SelectFields[i].ru);
    ItemIndex := 7;
    OnSelect := @ChangeIndicate;
  end;

  ShowBtn := TButton.Create(SectionPanel);
  with ShowBtn do begin
    Parent := SectionPanel;
    Top := 88;
    Left := CB_Vert.Left;
    Width := 80;
    Caption := ' Показать ';
    OnClick := @ShowSchedule;
  end;

  Indicate := TImage.Create(SectionPanel);
  with Indicate do begin
    Parent := SectionPanel;
    Top := 84;
    Left := ShowBtn.Left + ShowBtn.Width + indent;
    Picture.LoadFromFile('yes.png');
  end;
end;

procedure TScheduleTable.ChangeIndicate(Sender: TObject);
begin
  Indicate.Visible := False;
end;

procedure TScheduleTable.CreateCheckPanel();
var
  i, r: integer;
  SchElemName: array [0..7] of string = ( 'Предмет', 'Тип занятий',
    'Преподаватель', 'Начало занятий', 'Окончание занятий', 'Группа',
    'Аудитория', 'День недели');
begin
  CheckPanel := TPanel.Create(ToolsPanel);
  with CheckPanel do begin
    Height := ToolsPanel.Height;
    Width := 300;
    Parent := ToolsPanel;
    Left := SectionPanel.Width;
  end;
  r := 0;
  for i := 0 to High(SchElemName) do begin
    CheckIndicate[i] := TCheckBox.Create(CheckPanel);
    with CheckIndicate[i] do begin
      if i mod 2 = 0 then begin
        Left := 18;
        r += 1;
      end else begin
        Left := 160;
      end;
      Top := 18 * r;
      Caption := SchElemName[i];
      Parent := CheckPanel;
      Checked := True;
      OnClick := @CheckClick;
    end;
  end;
end;

procedure TScheduleTable.CheckClick(Sender: TObject);
begin
  ScheduleGrid.Invalidate;
end;

procedure TScheduleTable.CreatePreferPanel();
var
  Check: TCheckBox;
begin
  PreferPanel := TPanel.Create(ToolsPanel);
  with PreferPanel do begin
    Height := ToolsPanel.Height;
    Left := CheckPanel.Left + CheckPanel.Width;
    Width := 300;
    Parent := ToolsPanel;
  end;
  Check := TCheckBox.Create(PreferPanel);
  with Check do begin
    Top := 18;
    Left := 18;
    Caption := 'Показывать заголовки';
    Parent := PreferPanel;
    Checked := True;
    OnClick := @PreferClick;
  end;
  ShowHeads := True;
  Check := TCheckBox.Create(PreferPanel);
  with Check do begin
    Top := 36;
    Left := 18;
    Caption := 'Показывать все элементы расписания';
    Parent := PreferPanel;
    Checked := False;
    OnClick := @ShowAllClick;
  end;
  ShowAll := False;
end;

procedure TScheduleTable.PreferClick(Sender: TObject);
begin
  ShowHeads := not ShowHeads;
  ScheduleGrid.Invalidate;
end;

procedure TScheduleTable.ShowAllClick(Sender: TObject);
begin
  ShowAll := not ShowAll;
  ShowSchedule(Self);
end;

function TScheduleTable.GetFieldName(AFieldName: string): string;
begin
  Result := DBTools.DataSource.DataSet.FieldByName(AFieldName).Value;
  DBTools.DataSource.DataSet.Next;
end;

function TScheduleTable.GetCountFrom(idx: integer):integer;
var
  s: TForeignKey;
begin
  s := Table.Fields[idx].ForeignKey;
  DBTools.ExecuteQuery('SELECT COUNT(' + s.FieldName + ') FROM ' + s.Table.Name);
  Result := StrToInt(DBTools.DataSource.DataSet.FieldByName('COUNT').Text);
end;

procedure TScheduleTable.ShowSchedule(Sender: TObject);
var
  i: integer;
begin
  Indicate.Visible := True;
  for i := 0 to High(CheckIndicate) do
    CheckIndicate[i].Checked := True;
  CheckIndicate[CB_Horz.ItemIndex].Checked := False;
  CheckIndicate[CB_Vert.ItemIndex].Checked := False;
  FilterPanel.SelectText := 'SELECT ' +
    Table.SelectFields[CB_Horz.ItemIndex + 1].OrderID + ', ' +
    Table.SelectFields[CB_Vert.ItemIndex + 1].OrderID + ', ' +
    Table.FieldsList() + ' FROM ' + Table.JoinName;;
  FilterPanel.OrderByText :=
    ' ORDER BY ' + Table.SelectFields[CB_Horz.ItemIndex + 1].OrderID + ', ' +
    Table.SelectFields[CB_Vert.ItemIndex + 1].OrderID;
  FilterPanel.ShowBtnClick(Self);
end;

procedure TScheduleTable.UpdateScheduleGrid();
var
  i: integer;
  ForeignKey: UTables.TForeignKey;
  q: TMyDBTools;
begin
  SetLength(ScheduleMatrix, 0, 0, 0);
  SetLength(ShowElemOfCell, 0, 0);
  q := TMyDBTools.Create(Self, 9);
  with ScheduleGrid do begin
    Options := Options + [goSmoothScroll];
    ColCount := GetCountFrom(CB_Horz.ItemIndex + 1) + 1;
    RowCount := GetCountFrom(CB_Vert.ItemIndex + 1) + 1;
    SetLength(ScheduleMatrix, ColCount, RowCount, 100);
    SetLength(ShowElemOfCell, ColCount, RowCount);
    Canvas.Pen.Color := clBlack;
    ForeignKey := Table.Fields[CB_Horz.ItemIndex + 1].ForeignKey;
    q.ExecuteQuery('SELECT * FROM ' + ForeignKey.Table.Name +
      ' ORDER BY ' + ForeignKey.Table.OrderBy);
    for i := 1 to ColCount - 1 do begin
      ColWidths[i] := CWidth;
      ScheduleMatrix[i][0][0].Header :=
        q.SQLQuery.FieldByName(ByName(ForeignKey.FieldName)).Value;
      q.SQLQuery.Next;
    end;
    ForeignKey := Table.Fields[CB_Vert.ItemIndex + 1].ForeignKey;
    q.ExecuteQuery('SELECT * FROM ' + ForeignKey.Table.Name +
      ' ORDER BY ' + ForeignKey.Table.OrderBy);
    for i := 1 to RowCount - 1 do begin
      RowHeights[i] := RHeight;
      ScheduleMatrix[0][i][0].Header :=
        q.SQLQuery.FieldByName(ByName(ForeignKey.FieldName)).Value;
      q.SQLQuery.Next;
    end;
  end;
  q.Free; q :=  nil;
  if ShowHeads then begin
    ScheduleGrid.ColWidths[0] :=
      Table.SelectFields[CB_Vert.ItemIndex + 1].Width + 18;
    ScheduleGrid.RowHeights[0] := RHeight_h;
  end;
end;

procedure TScheduleTable.FillScheduleMatrix();
var
  i, j, k, r: integer;
begin
  for i := 1 to ScheduleGrid.ColCount - 1 do begin
    for j := 1 to ScheduleGrid.RowCount - 1 do begin
      r := 0;
      while (not DBTools.DataSource.DataSet.EOF) and
            (DBTools.DataSource.DataSet.Fields[CB_Horz.ItemIndex + 3].Value
               = ScheduleMatrix[i][0][0].Header) and
            (DBTools.DataSource.DataSet.Fields[CB_Vert.ItemIndex + 3].Value
               = ScheduleMatrix[0][j][0].Header)
      do begin
        ScheduleMatrix[i][j][r].ID :=
          DBTools.DataSource.DataSet.Fields[2].AsString;
        for k := 0 to DBTools.DataSource.DataSet.FieldCount - 4 do
          ScheduleMatrix[i][j][r].SchElemField[k] :=
            DBTools.DataSource.DataSet.Fields[k + 3].AsString;
        DBTools.DataSource.DataSet.Next;
        r += 1;
      end;
      SetLength(ScheduleMatrix[i][j], r + 1);
      if ShowAll and (ScheduleGrid.RowHeights[j] < r* RHeight)then
        ScheduleGrid.RowHeights[j] := r * RHeight;
    end;
  end;
  ScheduleGrid.Invalidate;
end;

procedure TScheduleTable.SetCellHeight(aRow, r: integer);
begin
  ScheduleGrid.RowHeights[aRow] := (r + 1)* RHeight;
end;

procedure TScheduleTable.DrawGridDrawCell(
  Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState:TGridDrawState);
var
  i, r: integer;
  Head: string;
  Indents: array [0..7, 0..1] of integer = (
    (18, 18), (210, 54), (18, 36) , (18, 72),
    (62, 72), (210, 36), (210, 72), (18, 54));
  SchElemName: array [0..7] of string = ( 'Предмет: ', 'Тип занятий: ',
    'Преподаватель: ', '', ' - ', 'Группа: ', 'Аудитория: ', 'День недели: ');
begin
  if (aCol = 0) and (aRow = 0) then begin
    ScheduleGrid.Canvas.Pen.Color := clBlack;
    ScheduleGrid.Canvas.Line(aRect.TopLeft, aRect.BottomRight);
    Exit;
  end;
  if (aCol = 0) or (aRow = 0) then begin
    with ScheduleGrid.Canvas do begin
      Font.Style := [fsBold];
      TextOut(aRect.Left + Indents[0, 0], aRect.Top + Indents[0, 1],
        ScheduleMatrix[aCol][ARow][0].Header);
    end;
    Exit;
  end;

  if ScheduleMatrix[aCol][aRow][0].SchElemField[0] = '' then Exit;
  r := 0;
  while (ScheduleMatrix[aCol][aRow][r].SchElemField[0] <> '') do begin
    with ScheduleGrid.Canvas do begin
      Rectangle(
        aRect.Left, aRect.Top + r * RHeight,
        aRect.Right, aRect.Bottom + r * RHeight);
      for i := 0 to High(CheckIndicate) do
        if CheckIndicate[i].Checked then begin
          if ShowHeads then
            Head := SchElemName[i]
          else
            Head := '';
          TextOut(
              aRect.Left + Indents[i][0],
              aRect.Top + Indents[i][1] + r * RHeight,
              Head + ScheduleMatrix[aCol][aRow][r].SchElemField[i]);
        end;
    end;
    r += 1;
  end;
  if (Length(ScheduleMatrix[aCol][aRow]) > 2) and
     (not ShowAll) and (not ShowElemOfCell[aCol][aRow]) then
    with ScheduleGrid.Canvas do begin
      Pen.Color := clBlack;
      Brush.Color := clYellow;
      Rectangle(
        aRect.BottomRight.X - 12, aRect.BottomRight.Y - 12,
        aRect.BottomRight.X, aRect.BottomRight.Y);
    end;
end;

end.

