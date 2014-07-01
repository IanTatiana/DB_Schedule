unit UTables;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Menus;

type
  TDataType = (int, varchar, date);

  TForeignTable = record
    Name: string;
    OrderBy: string;
  end;

  TForeignKey = record
    Table: TForeignTable;
    FieldName: string;
  end;

type
  TSortType = (stNatural, stLingvo, st);

  TField = record
    Visible: boolean;
    en, ru, OrderID: string;
    Width: integer;
    DateType: TDataType;
    ForeignKey: TForeignKey;
    SortType: TSortType;
  end;

  TTableType = (tCascade, tMarker, tUnChange, tJoin);

  { TTable }

  TTable = class
  protected
    procedure AddSelectField(AName, ARusName: string; AWidth: integer;
      ASortType: TSortType); overload;
    procedure AddSelectField(AName, AOrderID, ARusName: string; AWidth: integer); overload;
    procedure AddField(AVisible: boolean; AName: string;
      ADataType: TDataType; AForeignKey: TForeignKey); overload;
    procedure AddField(AVisible: boolean; AName: string;
      ADataType: TDataType); overload;
  public
    TableType: TTableType;
    Join: boolean;
    JoinName: string;
    en, ru: string;
    Fields: array of TField;
    SelectFields: array of TField;
    GenName: string;
    constructor Create(AName, ARusName, AGenName: string; AJoin: boolean;
      ATableDelType: TTableType);
    function FieldsList(): string;
    function GetPossibleValue(AField: TField): String;
  end;

  function ByName(AName: string): string;
  function ForeignKey(
    ATableName: string; AFieldName: string; ASortType: TSortType): TForeignKey;
  function GenNewTable(ATable1, ATable2,
  AField1, AField2: string; i: integer): string;

const
  null = '*null*';

var
  Menu: TMainMenu;
  Tables: array of TTable;
  ConflTable, Confl_ListTable, ConflTypeTable: TTable;

implementation

function ForeignKey(
  ATableName: string; AFieldName: string; ASortType: TSortType = st): TForeignKey;
begin
  with Result.Table do begin
    Name := ATableName;
    case ASortType of
      stLingvo: OrderBy := 'Name';
      stNatural: OrderBy := '"Index"';
      st: OrderBy := '';
    end;
  end;
  Result.FieldName := AFieldName;
end;

function GenNewTable(ATable1, ATable2,
  AField1, AField2: string; i: integer): string;
var
  CmpSgn: array [0..4] of string[10] = ('>', '<', '=', '>=', '<=');
begin
  Result := ATable1 + ' INNER JOIN ' + ATable2 +
    ' ON ' + AField1 + ' ' + CmpSgn[i] + ' ' + AField2;
end;

procedure TableCreate(AName, ARusName, AGenName: string; AJoin: boolean;
  ATableDelType: TTableType);
begin
  SetLength(Tables, Length(Tables) + 1);
  Tables[High(Tables)] := TTable.Create(AName, ARusName, AGenName, AJoin,
    ATableDelType);
end;

function ByName(AName: string): string;
begin
  Result := AName;
  if Pos('"', Result) = 1 then begin
    Delete(Result, 1, 1);
    Delete(Result, Length(Result), 1);
  end;
end;

//----TTable--------------------------------------------------------------------
constructor TTable.Create(AName, ARusName, AGenName: string; AJoin: boolean;
  ATableDelType: TTableType);
begin
  en := AName;
  ru := ARusName;
  GenName := AGenName;
  Join := AJoin;
  TableType := ATableDelType;
end;

procedure TTable.AddSelectField(AName, ARusName: string;
  AWidth: integer; ASortType: TSortType);
begin
  SetLength(SelectFields, Length(SelectFields) + 1);
  with SelectFields[High(SelectFields)] do begin
    en := AName;
    ru := ARusName;
    Width := AWidth;
    if ASortType = stNatural then
      OrderID := '"Index"';
    if ASortType = stLingvo then
      OrderID := 'Name';
  end;
end;

procedure TTable.AddSelectField(AName, AOrderID, ARusName: string;
  AWidth: integer);
begin
  SetLength(SelectFields, Length(SelectFields) + 1);
  with SelectFields[High(SelectFields)] do begin
    en := AName;
    ru := ARusName;
    OrderID := AOrderID;
    Width := AWidth;
  end;
end;

procedure TTable.AddField(AVisible: boolean; AName: string;
  ADataType: TDataType; AForeignKey: TForeignKey);
begin
  SetLength(Fields, Length(Fields) + 1);
  with Fields[High(Fields)] do begin
    Visible := AVisible;
    en := AName;
    DateType := ADataType;
    ForeignKey := AForeignKey;
  end;
end;

procedure TTable.AddField(AVisible: boolean; AName: string;
  ADataType: TDataType);
begin
  AddField(AVisible, AName, ADataType, ForeignKey(null, null, st));
end;

function TTable.FieldsList(): string;
var
  i: integer;
begin
  Result := SelectFields[0].en;
  for i := 1 to High(SelectFields) do
    Result += ', ' + SelectFields[i].en;
end;

function TTable.GetPossibleValue(AField: TField): String;
begin
  if AField.ForeignKey.Table.Name = null then
    Result := 'SELECT ' + AField.en + ' FROM ' + en
  else
    Result := 'SELECT ' + AField.ForeignKey.FieldName + ' FROM ' +
      AField.ForeignKey.Table.Name;
end;

//------------------------------------------------------------------------------
procedure RegisterMetaData();
begin
  TableCreate('Days', 'Дни недели', 'DAY_INDEX', False, tUnChange);
  with Tables[High(Tables)] do begin
    JoinName := en;
    AddSelectField('"Index"', 'Index', 100, stNatural);
    AddSelectField('Name', 'День недели', 100, stNatural);
    AddField(False, '"Index"', int);
    AddField(True, 'Name', varchar);
  end;
  TableCreate('Groups', 'Список групп', 'GROUP_ID', False, tCascade);
  with Tables[High(Tables)] do begin
    JoinName := en;
    AddSelectField('ID', 'ID', 100, stLingvo);
    AddSelectField('Name', 'Группа', 100, stLingvo);
    AddSelectField('Group_size', 'Кол-во человек', 100, stLingvo);
    AddField(False, 'ID', int);
    AddField(True, 'Name', varchar);
    AddField(True, 'Group_size', int);
  end;
  TableCreate('Professors', 'Преподаватели', 'PROFESSOR_ID', False, tMarker);
  with Tables[High(Tables)] do begin
    JoinName := en;
    AddSelectField('ID', 'ID', 100, stLingvo);
    AddSelectField('Name', 'Имя', 150, stLingvo);
    AddField(False, 'ID', int);
    AddField(True, 'Name', varchar);
  end;
  TableCreate('Rooms', 'Аудитории', 'ROOM_ID', False, tCascade);
  with Tables[High(Tables)] do begin
    JoinName := en;
    AddSelectField('ID', 'ID', 100, stLingvo);
    AddSelectField('Name', 'Имя', 100, stLingvo);
    AddSelectField('"Size"', 'Вместимость', 90, stLingvo);
    AddField(False, 'ID', int);
    AddField(True, 'Name', varchar);
    AddField(True, '"Size"', int);
  end;
  TableCreate('Subjects', 'Предметы', 'SUBJECT_ID', False, tMarker);
  with Tables[High(Tables)] do begin
    JoinName := en;
    AddSelectField('ID', 'ID', 100, stLingvo);
    AddSelectField('Name', 'Предмет', 380, stLingvo);
    AddField(False, 'ID', int);
    AddField(True, 'Name', varchar);
  end;
  TableCreate('Times', 'Расписание звонков', 'TIME_INDEX', False, tCascade);
  with Tables[High(Tables)] do begin
    JoinName := en;
    AddSelectField('"Index"', 'Index', 100, stNatural);
    AddSelectField('"Begin"', 'Начало', 100, stNatural);
    AddSelectField('"End"', 'Конец', 100, stNatural);
    AddField(False, '"Index"', int);
    AddField(True, '"Begin"', date);
    AddField(True, '"End"', date);
  end;
  TableCreate('Subject_Types', 'Тип Занятий', 'TYPE_ID', False, tCascade);
  with Tables[High(Tables)] do begin
    JoinName := en;
    AddSelectField('ID', 'ID', 100, stLingvo);
    AddSelectField('Name', 'Тип', 50, stLingvo);
    AddField(False, 'ID', int);
    AddField(True, 'Name', varchar);
  end;
  TableCreate('Subjects_Groups', 'Предметы(по группам)', 'SG_ID', True, tJoin);
  with Tables[High(Tables)] do begin
    JoinName := GenNewTable('Subjects_Groups sg', 'Subjects s',
      'sg.Subject_ID', 's.id', 2);
    JoinName := GenNewTable(JoinName, 'Groups g', 'sg.Group_ID', 'g.id', 2);
    AddSelectField('sg.ID', 'sg.ID', 'ID', 100);
    AddSelectField('s.name', 's.name', 'Предметы', 380);
    AddSelectField('g.name', 'g.name', 'Группа', 60);
    AddField(False, 'ID', int);
    AddField(True, 'Subject_ID', int, ForeignKey('Subjects', 'Name', stLingvo));
    AddField(True, 'Group_ID', int, ForeignKey('Groups', 'Name', stLingvo));
  end;
  TableCreate('Professors_Subjects', 'Преподаватель/Предмет', 'PS_ID', True,
    tJoin);
  with Tables[High(Tables)] do begin
    JoinName := GenNewTable(
      'Professors p', 'Professors_Subjects ps', 'p.id', 'ps.professor_id', 2);
    JoinName := GenNewTable(
      JoinName, 'Subjects s', 's.id', 'ps.subject_id', 2);
    AddSelectField('ps.ID', 'ps.ID', 'ID', 100);
    AddSelectField('p.name', 'p.name', 'Преподаватель', 110);
    AddSelectField('s.name', 's.name', 'Предмет', 380);
    AddField(False, 'ID', int);
    AddField(True, 'professor_id', int,
      ForeignKey('Professors', 'Name', stLingvo));
    AddField(True, 'subject_id', int,
      ForeignKey('Subjects', 'Name', stLingvo));
  end;
  TableCreate('Schedule_Items', 'Расписание', 'ITEM_ID', True, tJoin);
  with Tables[High(Tables)] do begin
    JoinName := GenNewTable(
      'Subjects s', 'Schedule_Items sch', 's.id', 'sch.subject_id', 2);
    JoinName := GenNewTable(
      JoinName, 'Subject_Types st', 'st.id', 'sch.Subject_Type_ID', 2);
    JoinName := GenNewTable(
      JoinName, 'Professors p', 'p.id', 'sch.professor_id', 2);
    JoinName := GenNewTable(
      JoinName, 'Times ts', 'ts."Index"', 'sch.Time_Index', 2);
    JoinName := GenNewTable(
      JoinName, 'Days d', 'd."Index"', 'sch.Day_Index', 2);
    JoinName := GenNewTable(
      JoinName, 'Groups g', 'g.id', 'sch.group_id', 2);
    JoinName := GenNewTable(
      JoinName, 'Rooms r', 'r.id', 'sch.room_id', 2);
    AddSelectField('sch.ID', 'sch.ID', 'ID', 100);
    AddSelectField('s.name', 's.name', 'Предмет', 380);
    AddSelectField('st.name', 'st.name', 'Тип занятий', 100);
    AddSelectField('p.name', 'p.name', 'Преподаватель', 120);
    AddSelectField('ts."Begin"', 'ts."Index"', 'Начало', 100);
    AddSelectField('ts."End"', 'ts."Index"', 'Конец', 100);
    AddSelectField('g.name', 'g.name', 'Группа', 60);
    AddSelectField('r.name', 'r.name', 'Аудитория', 100);
    AddSelectField('d.name', 'd."Index"', 'День недели', 120);
    AddField(False, 'ID', int,
      ForeignKey(null, null, st));
    AddField(True, 'Subject_ID', int,
      ForeignKey('Subjects', 'Name', stLingvo));
    AddField(True, 'Subject_Type_ID', int,
      ForeignKey('Subject_Types', 'Name', stLingvo));
    AddField(True, 'Professor_ID', int,
      ForeignKey('Professors', 'Name', stLingvo));
    AddField(True, 'Time_Index', int,
      ForeignKey('Times', '"Begin"', stNatural));
    AddField(True, 'Day_Index', int,
      ForeignKey('Times', '"End"', stNatural));
    AddField(True, 'Group_ID', int,
      ForeignKey('Groups', 'Name', stLingvo));
    AddField(True, 'Room_ID', int,
      ForeignKey('Rooms', 'Name', stLingvo));
    AddField(True, 'Week', int,
      ForeignKey('Days', 'Name', stNatural));
  end;

  ConflTable := TTable.Create(
    'Conflicts', 'Конфликты', 'CONFL_ID', False, tUnChange);
  with ConflTable do begin
    JoinName := ConflTable.en;
    AddSelectField('ID', 'ID', 100, stNatural);
    AddSelectField('Name', 'Название', 100, stNatural);
    AddField(False, 'ID', int);
    AddField(True, 'Name', varchar);
  end;
  ConflTypeTable := TTable.Create(
    'Confl_Type', 'Категории конфликтов', 'CONFL_TYPE_ID', False, tUnChange);
  with ConflTypeTable do begin
    JoinName := ConflTypeTable.en;
    AddSelectField('ID', 'ID', 100, stNatural);
    AddSelectField('Name', 'Тип конфликта', 100, stNatural);
    AddField(False, 'ID', int);
    AddField(True, 'Name', varchar);
  end;
  Confl_ListTable := TTable.Create(
    'Confl_List', 'Кофликтующие записи', 'CONFLLIST_ID', True, tJoin);
  with Confl_ListTable do begin
    JoinName := GenNewTable(
      'Confl_List CL', 'Conflicts C',  'CL.Confl_ID', 'C.ID', 2);
    JoinName := GenNewTable(
      JoinName, 'Confl_Type CT', 'CL.Confl_Type_ID', 'CT.ID', 2);
    AddSelectField('CL.ID', 'CL.ID', 'ID', 100);
    AddSelectField('C.Name', 'C.Name', 'Конфликт', 100);
    AddSelectField('CT.Name', 'CT.Name', 'Тип конфликта', 120);
    AddField(False, 'ID', int,
      ForeignKey(null, null, st));
    AddField(True, 'Confl_ID', int,
      ForeignKey('Conflicts', 'Name', stNatural));
    AddField(True, 'Confl_Type_ID', int,
      ForeignKey('Confl_Type', 'Name', stNatural));
  end;
end;

initialization
  RegisterMetaData;

end.

