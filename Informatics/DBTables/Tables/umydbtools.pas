unit UMyDBTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, sqldb, IBConnection, UTables, Dialogs;

type
  TMyDBTools = class
  public
    DataSource: TDataSource;
    SQLQuery: TSQLQuery;
    constructor Create(AOwner: TComponent; Num: integer);
    function GetValue(i: integer): string;
    procedure ExecuteQuery(AQuery: string);
    procedure ExecuteParamQuery(AQuery: string; i: integer);
    procedure EditQuery(AEditQuery: string);
    destructor Destroy; override;
  end;

var
  IBConnection_: TIBConnection;
  SQLTransaction_: TSQLTransaction;

implementation

constructor TMyDBTools.Create(AOwner: TComponent; Num: integer);
begin
  Inherited Create;
  SQLQuery := TSQLQuery.Create(AOwner);
  with SQLQuery do begin
    DataBase := IBConnection_;
    Transaction := SQLTransaction_;
    SQL.Text := 'SELECT * FROM ' + Tables[Num].en;
    Active := True;
  end;
  DataSource := TDataSource.Create(AOwner);
  DataSource.DataSet := SQLQuery;
end;

destructor TMyDBTools.Destroy;
begin
  DataSource.Free; DataSource := nil;
  SQLQuery.Free; SQLQuery := nil;
  inherited Destroy;
end;

procedure TMyDBTools.ExecuteQuery(AQuery: string);
begin
  SQLQuery.Active := False;
  SQLQuery.SQL.Text := AQuery;
  try
    SQLQuery.Active := True;
  except
    on E: EDatabaseError do begin
      ShowMessage('Не удалось выполнить запрос ' + E.Message);
      Exit;
    end;
  end;
end;

procedure TMyDBTools.ExecuteParamQuery(AQuery: string; i: integer);
begin

end;

procedure TMyDBTools.EditQuery(AEditQuery: string);
begin
  with SQLQuery do begin
    Active := False;
    SQL.Clear;
    SQL.Add(AEditQuery);
    ExecSQL;
  end;
end;

function TMyDBTools.GetValue(i: integer): string;
begin
  Result := DataSource.DataSet.Fields[i].Value;
  DataSource.DataSet.Next;
end;
{static int lua_sqlite_exec(lua_State *L){
	if (!lua_gettop(L) || !lua_isstring(L, 1)){
		printf("Please enter 1 or more string argumemts");
	} else {
		if (sqlite3_exec(lua_isstring(L, 1), &db)){
			printf("Can't open database");
		}
	}
	return 0;
}}
end.

