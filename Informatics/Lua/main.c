#define _CRT_SECURE_NO_WARNINGS
#include <stdio.h>
#include "Lua_sqlite_unit.h"
#include "lualib.h"
#include "lauxlib.h"
#include "lua.h"

void ShowErrMsg(lua_State *L){
	printf("\nFATAL ERROR:\n> %s\n\n", lua_tostring(L, -1));
}

int main(){
	lua_State *L = luaL_newstate();
	luaL_openlibs(L);

	if (luaL_loadfile(L, "test.lua"))
		ShowErrMsg(L);
	
	luaopen_sqlite(L);

	if (lua_pcall(L, 0, 0, 0))
		ShowErrMsg(L);
		
	system("pause");
}