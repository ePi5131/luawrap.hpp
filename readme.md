# luawrap.hpp
(C) 2023 ePi

## これなに？
C++らしく快適にLuaを書くために作ったシングルヘッダラッパーライブラリ<br>
C++23/Lua5.1を要求

## tips

### エラー処理
Luaのエラーは例外を投げるようにしています 外側でcatchをしないと大変なことになりそう<br>
LUA_CFUNCTIONを作るときはこんな感じにやるといい感じになってくれます
```cpp
int func(lua_State* _l) {
    Lua::State L{_l};
    try {
        // ...
        return 0;
    }
    catch (const Lua::Exception& e) {
        L.push(e);
    }
    L.error();
}
```

## ライセンス
MITにします

## その他
- 5.4用とか対応いる？誰か適当にやっといてください
- https://github.com/ThePhD/sol2
  - おい
  - いましりました
