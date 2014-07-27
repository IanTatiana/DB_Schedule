#include "lualib.h"
#include "lauxlib.h"
#include "lua.h"

#include "sqlite3.h"

char convert(lua_State *L){
	if (!lua_isstring(L, 1)){
		printf("\nThe argument isn't string\n");
		return 0;
	}
	return 1;
}

sqlite3 *db;

static int lua_sqlite3_open(lua_State *L){
	if (!convert(L)) return 0;

	if (sqlite3_open(lua_tostring(L, 1), &db) != SQLITE_OK){
		printf("\nCan't open database: %s\n", sqlite3_errmsg(db));
		return 0;
	} else 
		printf("\nOpened database %s\n", lua_tostring(L, 1));
	return 1;
}

char listcnt;

static int callback(void *context, int colcnt, char **values, char **names){
	int i;

	if (!listcnt){
		printf("\n");
		for (i = 0; i <= colcnt; ++i){
			if (i == colcnt)
				printf("|\t");
			else
				printf("|%s\t", names[i]);
		}
		printf("\n");
		++listcnt;
	}
	
	for (i = 0; i <= colcnt; ++i){
		if (i == colcnt)
			printf("|\t");
		else
			printf("|%s\t", values[i]);
	}
	printf("\n");

	return 0;
}

static int lua_sqlite3_exec(lua_State *L){
	if (!convert(L)) return 0;

	char *SQLError;
	if (sqlite3_exec(db, lua_tostring(L, 1), callback, 0, &SQLError) != SQLITE_OK){
		printf("\nSQL Error: %s\n", SQLError);
		sqlite3_free(db);
	}
	return 1;
}

static int lua_sqlite3_close(lua_State *L){
	if (!convert(L)) return 0;

	if (sqlite3_close(db) != SQLITE_OK){
		printf("\nCan't close database %s\n", lua_tostring(L, 1));
	} else
		printf("\nClosed database %s\n", lua_tostring(L, 1));
}

void luaopen_sqlite(lua_State *L){
	lua_register(L, "sqlite_open", lua_sqlite3_open);
	lua_register(L, "sqlite_exec", lua_sqlite3_exec);
	lua_register(L, "sqlite_close", lua_sqlite3_close);
}