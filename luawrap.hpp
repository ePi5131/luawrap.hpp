/**
 * @file luawrap.hpp
 * @author ePi
 * @version 0.1
 */

#pragma once
#include <cassert>
#include <string>
#include <stdexcept>
#include <type_traits>
#include <optional>
#include <functional>
#include <variant>
#include <string_view>
#include <utility>
#include <filesystem>
#include <format>
#include <ranges>
#include <expected>
#include <array>
#include <span>

#include <lua.hpp>

namespace Lua {
    /// \brief LUA_VERSION
    inline constexpr auto Version = LUA_VERSION;

    /// \brief LUA_RELEASE
    inline constexpr auto Release = LUA_RELEASE;

    /// \brief LUA_VERSION_NUM
    inline constexpr auto VersionNum = LUA_VERSION_NUM;

    /// \brief LUA_COPYRIGHT
    inline constexpr auto Copyright = LUA_COPYRIGHT;

    /// \brief LUA_AUTHORS
    inline constexpr auto Authors = LUA_AUTHORS;

    /// \brief LUA_SIGNATURE
    inline constexpr auto Signature = LUA_SIGNATURE;

    /// \brief LUA_MULTRET
    /// \details 多値を返すことをマークする
    inline constexpr auto MultRet = LUA_MULTRET;

    /// \brief スタック上のインデックスを表す整数
    /// \details 疑似インデックス(負値で、絶対値がスタックトップから数えたインデックス)も可
    using Index = int;

    /// \brief スタック上のインデックスを表す整数
    /// \warning 疑似インデックス不可
    using IndexAbsolute = int;

    enum IndexConst : IndexAbsolute {
        /// \brief LUA_REGISTRYINDEX
        /// \details レジストリという特殊な場所に値を配置する
        RegistryIndex = LUA_REGISTRYINDEX,

        /// \brief LUA_ENVIRONINDEX
        /// \details 「グローバル変数」がある場所
        EnvironIndex = LUA_ENVIRONINDEX,

        /// \brief LUA_GLOBALSINDEX
        /// \details インデックス <tt>GlobalsIndex - i</tt> がi番目の上位値になる
        GlobalsIndex = LUA_GLOBALSINDEX
    };

    /// \brief lua_upvalueindex
    inline constexpr IndexAbsolute upvalueIndex(int i) noexcept {
        return GlobalsIndex - i;
    }
    
    inline namespace literals {
        /// \brief lua_upvalueindex に相当するリテラル演算子
        /// @param i 1-indexed な上位値のインデックスを表す整数
        /// @return 上位値として解釈される IndexAbsolute
        inline consteval IndexAbsolute operator""_upvalue(unsigned long long i) {
            if (i == 0) {
                throw "i は1以上です";
            }
            if (!(std::in_range<int>(i) && std::in_range<int>(i + (-GlobalsIndex)))) {
                throw "i が大きすぎます";
            }
            return upvalueIndex(static_cast<int>(i));
        }
    }


    enum class Status : int {
        Ok,
        /// LUA_YIELD
        Yield = LUA_YIELD,
        /// LUA_ERRRUN
        ErrRun = LUA_ERRRUN,
        /// LUA_ERRSYNTAX
        ErrSyntax = LUA_ERRSYNTAX,
        /// LUA_ERRMEM
        ErrMem = LUA_ERRMEM,
        /// LUA_ERRERR
        ErrErr = LUA_ERRERR,
        /// LUA_ERRFILE
        ErrFile = LUA_ERRFILE
    };
    inline constexpr bool operator==(Status t, std::underlying_type_t<Status> i) { return std::to_underlying(t) == i; }
    
    inline constexpr std::string_view statusToStr(int s) {
        constexpr std::string_view map[] = {
            "Ok",
            "Yield",
            "ErrRun",
            "ErrSyntax",
            "ErrMem",
            "ErrErr",
            "ErrFile"
        };
        assert(0 <= s && s < std::ranges::ssize(map));
        return map[s];
    }
    inline constexpr auto to_string(Status s) {
        return statusToStr(static_cast<int>(s));
    }


    // lua_CFunction
    using CFunction = int(__cdecl*)(lua_State*);

    // lua_Reader
    using Reader = lua_Reader;

    // lua_Writer
    using Writer = lua_Writer;

    // lua_Alloc
    using Alloc = lua_Alloc;

    // LUA_T*
    enum class Type: int {
        /// LUA_TNONE
        None          = LUA_TNONE,
        /// LUA_TNIL
        Nil           = LUA_TNIL,
        /// LUA_TBOOLEAN
        Boolean       = LUA_TBOOLEAN,
        /// LUA_TLIGHTUSERDATA
        LightUserdata = LUA_TLIGHTUSERDATA,
        /// LUA_TNUMBER
        Number        = LUA_TNUMBER,
        /// LUA_TSTRING
        String        = LUA_TSTRING,
        /// LUA_TTABLE
        Table         = LUA_TTABLE,
        /// LUA_TFUNCTION
        Function      = LUA_TFUNCTION,
        /// LUA_TUSERDATA
        Userdata      = LUA_TUSERDATA,
        /// LUA_TTHREAD
        Thread        = LUA_TTHREAD
    };
    inline constexpr bool operator==(Type t, std::underlying_type_t<Type> i) { return std::to_underlying(t) == i; }

    // LUA_MINSTACK
    inline constexpr auto MinStack = LUA_MINSTACK;

    // lua_Integer
    using Integer = lua_Integer;

    // lua_Number
    using Number = lua_Number;
    

    enum class EventCode : int {
        Call, // LUA_HOOKCALL
        Ret, // LUA_HOOKRET
        Line, // LUA_HOOKLINE
        Count, // LUA_HOOKCOUNT
        TailRet // LUA_HOOKTAILRET
    };
    inline constexpr bool operator==(EventCode t, std::underlying_type_t<EventCode> i) { return std::to_underlying(t) == i; }

    inline constexpr std::string_view eventCodeToStr(int c) {
        constexpr std::string_view map[] = {
            "Call",
            "Ret",
            "Line",
            "Count",
            "TailRet"
        };
        assert(0 <= c && c < std::ranges::ssize(map));
        return map[c];
    }
    inline constexpr auto to_string(EventCode c) {
        return eventCodeToStr(std::to_underlying(c));
    }


    enum class EventMask : uint32_t {
        Call  = 1 << std::to_underlying(EventCode::Call),  // LUA_MASKCALL
        Ret   = 1 << std::to_underlying(EventCode::Ret),   // LUA_MASKRET
        Line  = 1 << std::to_underlying(EventCode::Line),  // LUA_MASKLINE
        Count = 1 << std::to_underlying(EventCode::Count), // LUA_MASKCOUNT
    };
    constexpr bool operator!(EventMask x) { return !std::to_underlying(x); }
    constexpr auto operator+(EventMask x) { return std::to_underlying(x); }
    constexpr EventMask operator~(EventMask x) { return EventMask{~std::to_underlying(x)}; }
    constexpr EventMask& operator&=(EventMask& x, EventMask y) { x = EventMask{std::to_underlying(x) & std::to_underlying(y)}; return x; }
    constexpr EventMask& operator|=(EventMask& x, EventMask y) { x = EventMask{std::to_underlying(x) | std::to_underlying(y)}; return x; }
    constexpr EventMask& operator^=(EventMask& x, EventMask y) { x = EventMask{std::to_underlying(x) ^ std::to_underlying(y)}; return x; }
    constexpr EventMask operator&(EventMask x, EventMask y) { x &= y; return x; }
    constexpr EventMask operator|(EventMask x, EventMask y) { x |= y; return x; }
    constexpr EventMask operator^(EventMask x, EventMask y) { x ^= y; return x; }

    constexpr size_t IdSize = LUA_IDSIZE;

    /// lua_Debug
    struct Debug {
        EventCode event;
        const char* name;       // n
        const char* namewhat;   // n
        const char* what;       // S
        const char* source;     // S
        int currentline;        // l
        int nups;               // u
        int linedefined;        // S
        int lastlinedefined;    // S
        std::array<char, IdSize> short_src; // S
    };

    /// lua_Hook
    using Hook = lua_Hook;

    /// luaL_Reg
    using Reg = luaL_Reg;

    class Exception : public std::runtime_error {
    public:
        Exception() : std::runtime_error{"(Empty Lua::Exception)"} {}
        explicit Exception(const std::string& mes) : std::runtime_error{mes} {}

        template<class... Args>
        Exception(std::format_string<Args...> fmt, Args&&... args):
            std::runtime_error{std::format(fmt, std::forward<Args>(args)...)}
        {}
    };

    class ArgException : public Exception {
    public:
        ArgException() : Exception{"(Empty Lua::ArgException)"} {}
        explicit ArgException(const std::string& mes) : Exception{mes.data()} {}
        
        ArgException(IndexAbsolute idx, std::string_view mes):
            Exception{"bad argument #{} ({})", idx, mes}
        {}

        template<class... Args>
        ArgException(std::format_string<Args...> fmt, Args&&... args):
            Exception{fmt, std::forward<Args>(args)...}
        {}
    };

    class TypeException : public Exception {
    public:
        TypeException() : Exception{"(Empty Lua::TypeException)"} {}
        explicit TypeException(const std::string& mes) : Exception{mes.data()} {}

        TypeException(IndexAbsolute narg, std::string_view expected, std::string_view got):
            Exception{"bad argument #{} ({} expected, got {})", narg, expected, got}
        {}

        template<class... Args>
        TypeException(std::format_string<Args...> fmt, Args&&... args):
            Exception{fmt, std::forward<Args>(args)...}
        {}
    };

    class State;

    template<class T>
    void luaClassRequire(State) noexcept {
        static_assert([]{return 0;}(), "luaClass の使用にはこの関数を特殊化します。");
    }
    
    template<class T>
    const char* luaClassName(State) noexcept {
        static_assert([]{return 0;}(), "luaClass の使用にはこの関数を特殊化します。");
    }

    template<Type t>
    [[nodiscard]] consteval std::string_view typeName() noexcept {
        using namespace std::string_view_literals;
        if constexpr (t == Type::Nil) {
            return "nil"sv;
        }
        else if constexpr (t == Type::Boolean) {
            return "boolean"sv;
        }
        else if constexpr (t == Type::LightUserdata || t == Type::Userdata) {
            return "userdata"sv;
        }
        else if constexpr (t == Type::Number) {
            return "number"sv;
        }
        else if constexpr (t == Type::String) {
            return "string"sv;
        }
        else if constexpr (t == Type::Table) {
            return "table"sv;
        }
        else if constexpr (t == Type::Function) {
            return "function"sv;
        }
        else if constexpr (t == Type::Thread) {
            return "thread"sv;
        }
        else {
            static_assert([]{return 0;}(), "無効な Typeです");
            std::unreachable();
        }
    }

    namespace { // begin private namespace
    namespace detail {
        inline constexpr const char* to_data(const char* ptr) noexcept { return ptr; }
        inline constexpr const char* to_data(std::string_view sv) noexcept { return sv.data(); }

        inline size_t length(const char* ptr) { return std::strlen(ptr); }
        inline size_t length(std::string_view sv) noexcept { return sv.size(); }

        inline constexpr std::string to_str(const char* ptr) { return std::string(ptr); }
        inline constexpr std::string to_str(std::string_view sv) { return std::string(sv); }
        
        inline constexpr const char* to_cstr(const char* ptr) noexcept { return ptr; }
        inline constexpr const char* to_cstr(const std::string& s) noexcept { return s.c_str(); }
    }
    } // end private namespace

    template<class T>
    concept StringArg = requires(T t) {
        detail::to_data(t);
    };

    // ゼロ終端
    template<class T>
    concept ZStringArg = requires(T t) {
        detail::to_cstr(t);
    };

    template<class T>
    concept PathArg =
        std::convertible_to<T, const char*> ||
        std::convertible_to<T, const std::string&> ||
        std::convertible_to<T, const std::filesystem::path&>;

    template<class T>
    concept RegRange = std::convertible_to<std::ranges::range_value_t<T>, const Reg&>;

    template<class T>
    concept loadInvocable = requires(T t, State l) {
        std::invoke_r<std::span<const char>>(t, l);
    };

    template<class T>
    concept dumpInvocable = requires(T t, State l, std::span<const std::byte> d) {
        std::invoke_r<int>(t, l, d);
    };

    class StackIndex;

    class State {
        lua_State* L;

        template<class T>
        static int __cdecl luaClassDtorDefault(lua_State* L) noexcept(std::is_nothrow_destructible_v<T>) {
            return static_cast<T*>(lua_touserdata(L, 1))->~T(), 0;
        }

    public:
        constexpr State(lua_State* L) noexcept : L{L} {}
        lua_State* getRawState() const noexcept { return L; }
        lua_State*& getRawState() noexcept { return L; }

        operator lua_State*() const noexcept { return L; }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

        static constexpr bool isAbsoluteIndex(Index idx) noexcept {
            return idx >= 0 || idx <= RegistryIndex;
        }

        static constexpr IndexAbsolute getAbsoluteIndex(Index idx, IndexAbsolute top) noexcept { return top + idx + 1; }
        IndexAbsolute getAbsoluteIndex(Index idx) const noexcept { return getAbsoluteIndex(idx, getTop()); }

        constexpr StackIndex operator[](Index idx) const noexcept;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        
        // luaL_newstate
        [[nodiscard]] explicit State() noexcept : L{luaL_newstate()} {}

        // lua_newstate
        [[nodiscard]] State(Alloc f, void* ud) : L(lua_newstate(f, ud)) {}

        // lua_close
        void close() const noexcept { lua_close(L); }

        // lua_newthread
        [[nodiscard]] State newThread() const noexcept { return { lua_newthread(L) }; }

        // lua_atpanic
        CFunction atPanic(CFunction panicf) const noexcept { return lua_atpanic(L, panicf); }

        // luaL_openlibs
        void openLibs() const noexcept { luaL_openlibs(L); }
        
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        
        /// lua_gettop
        [[nodiscard]] IndexAbsolute getTop() const noexcept { return lua_gettop(L); }

        /// lua_settop
        void setTop(Index idx) const noexcept { lua_settop(L, idx); }

        /// lua_pop
        void pop(std::make_unsigned_t<Index> n) const noexcept { return setTop(-static_cast<Index>(n) - 1); }

        /// lua_pushvalue
        void pushValue(Index idx) const noexcept { lua_pushvalue(L, idx); }

        /// lua_remove
        /// ST[idx..TOP-1]=ST[idx+1..TOP]; pop(1);
        void remove(Index idx) const noexcept { lua_remove(L, idx); }

        /// lua_insert
        /// ST[idx],ST[idx+1..TOP]=ST[TOP],ST[idx..TOP-1];
        void insert(Index idx) const noexcept { lua_insert(L, idx); }

        /// lua_replace
        /// ST[idx]=ST[top]; pop(1);
        void replace(Index idx) const noexcept { lua_replace(L, idx); }

        /// lua_checkstack
        [[nodiscard]] bool checkStack(int sz) const noexcept { return lua_checkstack(L, sz); }

        /// luaL_checkstack
        void checkStack(int sz, const StringArg auto& msg) const {
            if (!checkStack(sz)) {
                throw Exception{ detail::to_str(msg) };
            }
        }

        /// dst.push(ST[TOP-n+1..TOP]); pop(n);
        void xmove(const State& dst, int n) const noexcept { lua_xmove(L, dst.L, n); }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        
        /// lua_type
        [[nodiscard]] Type type(Index idx) const noexcept { return Type{ lua_type(L, idx) }; }
        
        /// lua_typename
        [[nodiscard]] const char* typeName(Type tp) const noexcept { return lua_typename(L, static_cast<int>(tp)); }

        /// luaL_typename
        [[nodiscard]] const char* typeName(Index idx) const noexcept { return typeName(type(idx)); }

        template<Type t>
        [[nodiscard]] inline static constexpr std::string_view typeName() noexcept { return Lua::typeName<t>(); }

        /// lua_isnone
        [[nodiscard]] bool isNone(Index idx) const noexcept { return type(idx) == Type::None; }

        /// lua_isnil
        [[nodiscard]] bool isNil(Index idx) const noexcept { return type(idx) == Type::Nil; }

        /// lua_isnoneornil
        [[nodiscard]] bool isNoneOrNil(Index idx) const noexcept { return static_cast<int>(type(idx)) <= 0; }
        
        /// lua_isboolean
        [[nodiscard]] bool isBoolean(Index idx) const noexcept { return type(idx) == Type::Boolean; }

        /// lua_isnumber とは異なる
        /// @return number型なら true
        [[nodiscard]] bool isNumber(Index idx) const noexcept { return type(idx) == Type::Number; }

        /// lua_isnumber
        /// @return number型か、numberに変換可能なstring型なら true
        [[nodiscard]] bool isNumberConvertable(Index idx) const noexcept { return lua_isnumber(L, idx); }

        /// lua_isstring とは異なり、実際にString型であることを検証する
        /// @return string型なら true
        [[nodiscard]] bool isString(Index idx) const noexcept { return type(idx) == Type::String; }

        /// lua_isstring
        /// @return number型かstring型なら true
        [[nodiscard]] bool isStringOrNumber(Index idx) const noexcept { return lua_isstring(L, idx); }

        /// lua_isfunction
        [[nodiscard]] bool isFunction(Index idx) const noexcept { return type(idx) == Type::Function; }

        /// lua_iscfunction
        [[nodiscard]] bool isCFunction(Index idx) const noexcept { return lua_iscfunction(L, idx); }

        /// Cの関数でない関数
        /// @return isFunction(idx) && !isCFunction(idx)
        [[nodiscard]] bool isLFunction(Index idx) const noexcept { return isFunction(idx) && !isCFunction(idx); }

        /// lua_islightuserdata
        [[nodiscard]] bool isLightUserdata(Index idx) const noexcept { return type(idx) == Type::LightUserdata; }

        /// lua_isuserdata
        [[nodiscard]] bool isUserdata(Index idx) const noexcept { return lua_isuserdata(L, idx); }

        /// @return type(idx) == Type::Userdata
        [[nodiscard]] bool isFullUserdata(Index idx) const noexcept { return type(idx) == Type::Userdata; }

        /// lua_istable
        [[nodiscard]] bool isTable(Index idx) const noexcept { return type(idx) == Type::Table; }

        /// lua_isthread
        [[nodiscard]] bool isThread(Index idx) const noexcept { return type(idx) == Type::Thread; }


        /// lua_toboolean
        /// @return Luaの意味論での真偽に同じ
        [[nodiscard]] bool toBoolean(Index idx) const noexcept { return lua_toboolean(L, idx); }

        /// lua_tonumber
        /// @return numberかnumberに変換可能なstringならその結果、さもなくば 0
        [[nodiscard]] Number toNumber(Index idx) const noexcept { return lua_tonumber(L, idx); }

        /// 失敗が戻り値に返る toNumber
        /// @return numberならば、toNumber の結果\n
        /// さもなくば、そのインデックスの Type
        [[nodiscard]] std::expected<Number, Type> tryToNumber(Index idx) const noexcept {
            if (const auto t = type(idx); t == Type::Number) {
                return toNumber(idx);
            }
            else {
                return std::unexpected{t};
            }
        }

        /// luaL_checknumber に相当
        /// @throw Lua::TypeException
        [[nodiscard]] Number checkNumber(IndexAbsolute narg) const {
            if (!isNumber(narg)) {
                throwTypeException<Type::Number>(narg);
            }
            return toNumber(narg);
        }

        /// luaL_optnumber に相当
        /// @throw Lua::TypeException
        [[nodiscard]] std::optional<Number> optNumber(Index narg) const noexcept {
            if (isNoneOrNil(narg)) return std::nullopt;
            return checkNumber(narg);
        }

        /// luaL_optnumber に相当
        /// @throw Lua::TypeException
        [[nodiscard]] Number optNumber(Index narg, Number d) const noexcept {
            if (isNoneOrNil(narg)) return d;
            return checkNumber(narg);
        }


        /// lua_tointeger
        /// @return numberかnumberに変換可能なstringならその結果、さもなくば 0
        [[nodiscard]] Integer toInteger(Index idx) const noexcept { return lua_tointeger(L, idx); }

        /// 失敗が戻り値に返る toInteger
        /// @return numberならば、toInteger の結果\n
        /// さもなくば、そのインデックスの Type
        [[nodiscard]] std::expected<Integer, Type> tryToInteger(Index idx) const noexcept {
            if (const auto t = type(idx); t == Type::Number) {
                return toInteger(idx);
            }
            else {
                return std::unexpected{t};
            }
        }

        /// luaL_checkinteger に相当
        /// @throw Lua::TypeException
        [[nodiscard]] Integer checkInteger(IndexAbsolute narg) const {
            if (!isNumber(narg)) {
                throwTypeException<Type::Number>(narg);
            }
            return toInteger(narg);
        }

        /// luaL_optinteger に相当
        /// @throw Lua::TypeException
        [[nodiscard]] std::optional<Integer> optInteger(IndexAbsolute narg) const {
            if (isNoneOrNil(narg)) return std::nullopt;
            return checkInteger(narg);
        }

        /// luaL_optinteger
        /// @throw Lua::TypeException
        [[nodiscard]] Integer optInteger(IndexAbsolute narg, Integer d) const {
            if (isNoneOrNil(narg)) return d;
            return checkInteger(narg);
        }


        /// lua_tostring, lua_tolstring
        /// @return stringならば、その内容\n
        /// numberならば、スタックのnumberをstringに変換した上で、その内容\n
        /// さもなくば、nullopt
        /// @warning この関数はスタックの値を変更することがある
        [[nodiscard]] std::optional<std::string_view> toString(Index idx) const noexcept {
            size_t size; const auto ptr = lua_tolstring(L, idx, &size);
            if (ptr == nullptr) return std::nullopt;
            return std::string_view{ptr, size};
        }

        /// stringであることをユーザーが保証した上で、その内容を取得する\n
        /// 必ず isString などで事前に検証をすること
        /// @warning この関数はスタックの値を変更することがある
        [[nodiscard]] std::string_view toStringTrusted(Index idx) const noexcept {
            size_t size; auto ptr = lua_tolstring(L, idx, &size);
            return {ptr, size};
        }

        /// スタックを編集しないtoString
        /// @return stringならば、その内容のコピー\n
        /// numberならば、Luaの方法でstringに変換した結果\n
        /// さもなくば、実際の型
        [[nodiscard]] std::expected<std::string, Type> toStringSafe(Index idx) const {
            switch (const auto t = type(idx)) {
                case Type::String:
                    return std::string{ toStringTrusted(idx) };
                case Type::Number: {
                    pushValue(-1);
                    std::string ret{ toStringTrusted(-1) };
                    pop(1);
                    return ret;
                }
                default:
                    return std::unexpected{t};
            }
        }

        /// 厳密にstringであるときのみ有効値を返す
        /// @return stringならば、その内容\n
        /// さもなくば、nullopt
        [[nodiscard]] std::optional<std::string_view> toStringStrict(Index idx) const noexcept {
            if (!isString(idx)) return std::nullopt;
            return toStringTrusted(idx);
        }

        /// luaL_checkstring, luaL_checklstring に相当
        /// @throw Lua::Exception
        /// @warning この関数はスタックの値を変更することがある
        [[nodiscard]] std::string_view checkString(IndexAbsolute idx) const {
            auto ret = toString(idx);
            if (!ret.has_value()) {
                throwTypeException<Type::String>(idx);
            }
            return ret.value();
        }

        /// スタックを編集しないcheckString
        /// @throw Lua::Exception
        [[nodiscard]] std::string checkStringSafe(IndexAbsolute idx) const {
            auto ret = toStringSafe(idx);
            if (!ret.has_value()) {
                throwTypeException<Type::String>(idx);
            }
            return ret.value();
        }

        /// 厳密にstringであることを要求する
        /// @throw Lua::TypeException
        [[nodiscard]] std::string_view checkStringStrict(IndexAbsolute idx) const {
            if (!isString(idx)) {
                throwTypeException(idx, "strict string");
            }
            return toStringTrusted(idx);
        }


        /// 失敗が戻り値に返る checkString
        /// @warning この関数はスタックの値を変更することがある
        [[nodiscard]] std::expected<std::string_view, Type> tryToString(IndexAbsolute idx) const noexcept {
            auto ret = toString(idx);
            if (!ret.has_value()) {
                return std::unexpected{type(idx)};
            }
            return ret.value();
        }

        /// 失敗が戻り値に返る checkStringSafe
        [[nodiscard]] std::expected<std::string, Type> tryToStringSafe(IndexAbsolute idx) const {
            auto ret = toStringSafe(idx);
            if (!ret.has_value()) {
                std::unexpected{type(idx)};
            }
            return ret.value();
        }

        /// 失敗が戻り値に返る checkStringStrict
        [[nodiscard]] std::expected<std::string_view, Type> tryToStringStrict(IndexAbsolute idx) const {
            if (!isString(idx)) {
                std::unexpected{type(idx)};
            }
            return toStringTrusted(idx);
        }
        

        /// luaL_optstring, luaL_optlstring
        /// @throw Lua::TypeException
        /// @warning この関数はスタックの値を変更することがある
        [[nodiscard]] std::optional<std::string_view> optString(Index narg) const {
            if (isNoneOrNil(narg)) return std::nullopt;
            return checkString(narg);
        }

        /// スタックを編集しない optString
        /// @throw Lua::TypeException
        [[nodiscard]] std::optional<std::string> optStringSafe(Index narg) const {
            if (isNoneOrNil(narg)) return std::nullopt;
            return checkStringSafe(narg);
        }

        /// 厳密にstringであることを要求する optString
        /// @throw Lua::TypeException
        [[nodiscard]] std::optional<std::string_view> optStringStrict(Index narg) const {
            if (isNoneOrNil(narg)) return std::nullopt;
            return checkStringStrict(narg);
        }


        /// lua_tocfunction
        [[nodiscard]] CFunction toCFunction(Index idx) const noexcept { return lua_tocfunction(L, idx); }

        // lua_touserdata
        // ついでにキャストもできる
        template<class T = void>
        [[nodiscard]] T* toUserdata(Index idx) const noexcept {
            return static_cast<T*>(lua_touserdata(L, idx));
        }

        // lua_tothread
        [[nodiscard]] State toThread(Index idx) const noexcept { return { lua_tothread(L, idx) }; }

        // lua_topointer
        [[nodiscard]] const void* toPointer(Index idx) const noexcept { return lua_topointer(L, idx); }


        /// lua_equal
        [[nodiscard]] int equal(Index idx1, Index idx2) const noexcept { return lua_equal(L, idx1, idx2); }

        /// lua_rawequal
        [[nodiscard]] bool rawEqual(Index idx1, Index idx2) const noexcept { return lua_rawequal(L, idx1, idx2); }
        
        /// lua_lessthan
        [[nodiscard]] bool lessThan(Index idx1, Index idx2) const noexcept { return lua_lessthan(L, idx1, idx2); }

        /// lua_objlen
        [[nodiscard]] size_t objLen(Index idx) const noexcept { return lua_objlen(L, idx); }

        /// lua_strlen
        [[nodiscard]] size_t strLen(Index idx) const noexcept { return objLen(idx); }

        /// luaL_checkany
        /// @throw ArgException
        void checkAny(IndexAbsolute narg) const {
            if (isNone(narg)) {
                throw ArgException{ narg, std::string_view("value expected") };
            }
        }

        /// luaL_checktype
        void checkType(IndexAbsolute narg, Type t) const noexcept { luaL_checktype(L, narg, static_cast<int>(t)); }

        /// luaL_checkoption
        [[nodiscard]] int checkOption(IndexAbsolute narg, const ZStringArg auto& def, const char* const lst[]) const noexcept { return luaL_checkoption(L, narg, detail::to_cstr(def), lst); }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

        [[nodiscard]] std::vector<Number> checkArrayOfNumber(Index idx) const {
            checkType(idx, Type::Table);
            const IndexAbsolute aidx = isAbsoluteIndex(idx) ? idx : getAbsoluteIndex(idx);
            std::vector<Number> ret;
            for (int c = 1;;c++) {
                push(c);
                getTable(aidx);
                switch (const auto t = type(-1)) {
                    case Type::Number:
                        ret.push_back(toNumber(-1));
                        pop(1);
                        break;
                    case Type::Nil:
                        pop(1);
                        return ret;
                    default:
                        pop(1);
                        throw Exception{"[{}] value type was {} (number expected)", c, typeName(t)};
                }
            }
        }

        [[nodiscard]] std::vector<std::string_view> checkArrayOfString(Index idx) const {
            checkType(idx, Type::Table);
            const IndexAbsolute aidx = isAbsoluteIndex(idx) ? idx : getAbsoluteIndex(idx);
            std::vector<std::string_view> ret;
            for (int c = 1;;c++) {
                push(c);
                getTable(aidx);
                switch (auto t = type(-1)) {
                    case Type::String:
                        ret.push_back(toStringTrusted(-1));
                        pop(1);
                        break;
                    case Type::Nil:
                        pop(1);
                        return ret;
                    default:
                        pop(1);
                        throw Exception{"[{}] value type was {} (string expected)", c, typeName(t)};
                }
            }
        }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        
        /// lua_pushnil
        void pushNil() const noexcept { lua_pushnil(L); }

        /// lua_pushnumber
        void pushNumber(Number n) const noexcept { lua_pushnumber(L, n); }

        /// lua_pushinteger
        void pushInteger(Integer n) const noexcept { lua_pushinteger(L, n); }
        
        /// lua_pushstring
        void pushString(const char* p) const noexcept { lua_pushstring(L, p); }

        /// lua_pushstring, lua_pushlstring
        void pushString(const std::string& s) const noexcept {
            lua_pushlstring(L, s.data(), s.size());
        }

        /// lua_pushstring, lua_pushlstring
        void pushString(std::string_view sv) const noexcept {
            lua_pushlstring(L, sv.data(), sv.size());
        }

        /// lua_pushstring, lua_pushlstring
        void pushString(const std::filesystem::path& p) const noexcept {
            const auto& s = p.string();
            lua_pushlstring(L, s.data(), s.size());
        }

        /// lua_pushfstring, lua_pushvfstring に相当\n
        /// フォーマットは std::format の文法で行う
        //template<class... Args>
        //requires (sizeof...(Args) > 0u)
        //void pushString(std::format_string<Args...> s, Args&&... args) const {
        //    pushString(std::format(s, std::forward<Args>(args)...));
        //}

        /// lua_pushcclosure
        void pushCClosure(int n, CFunction fn) const noexcept { lua_pushcclosure(L, fn, n); }

        /// lua_pushcfunction
        void pushCFunction(CFunction fn) const noexcept { pushCClosure(0, fn); }

        /// lua_pushcclosure
        void pushCFunction(int n, CFunction fn) const noexcept { pushCClosure(n, fn); }

        /// lua_pushboolean
        void pushBoolean(bool b) const noexcept { lua_pushboolean(L, b); }

        /// lua_pushlightuserdata
        void pushLightUserdata(void* p) const noexcept { lua_pushlightuserdata(L, p); }

        /// lua_pushthread
        int pushThread() const noexcept { return lua_pushthread(L); }

        void pushException(const Exception& e) const noexcept {
            pushString(e.what());
        }

    private:
        template<class F, class... Args>
        requires (
            std::constructible_from<F, Args...> &&
            requires(F f) {
                std::invoke_r<int>(f, std::declval<State>());
            }
        )
        F* pushFunctional_impl(Args&&... args) const {
            auto obj = newUserdata<F>(std::forward<Args>(args)...);
            if (!obj)return nullptr;

            if constexpr (!std::is_trivially_destructible_v<F>) {
                using namespace std::string_view_literals;
                newTable(0, 1);
                setTable(-1, "__gc"sv, &luaClassDtorDefault<F>);
                setMetatable(-2);
            }

            pushCClosure(1, [](lua_State* l) -> int {
                State L{l};
                auto& obj = *L.toUserdata<F>(1_upvalue);
                try {
                    return std::invoke_r<int>(obj, L);
                }
                catch (Exception& e) {
                    L.push(e);
                }
                L.error();
            });

            return obj;
        }

    public:
        template<class F>
        F* pushFunctional(F&& func) const
        requires requires(State L) {
            std::invoke_r<int>(func, L);
        } {
            return pushFunctional_impl<F>(std::forward<F>(func));
        }

        template<class F, class... Args>
        F* emplaceFunctional(Args&&... args) const
        requires (
            std::constructible_from<F, Args...> &&
            requires(F f) {
                std::invoke_r<int>(f, std::declval<State>());
            }
        ) {
            return pushFunctional_impl<F>(std::forward<Args>(args)...);
        }

        void push() const noexcept { pushNil(); }
        void push(bool value) const noexcept { pushBoolean(value); }
        void push(Integer value) const noexcept { pushInteger(value); }
        void push(Number value) const noexcept { pushNumber(value); }
        void push(const char* value) const noexcept { pushString(value); }
        void push(const std::string& value) const noexcept { pushString(value); }
        void push(std::string_view value) const noexcept { pushString(value); }
        void push(const std::filesystem::path& value) const noexcept { pushString(value); }
        void push(CFunction value) const noexcept { pushCFunction(value); }
        void push(void* value) const noexcept { pushLightUserdata(value); }

        void push(std::nullopt_t) const noexcept { pushNil(); }
        void push(std::monostate) const noexcept { pushNil(); }
        void push(const Exception& e) const noexcept { pushException(e); }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

        /// lua_gettable
        /// push [idx][ST[top]]
        void getTable(Index idx) const noexcept { lua_gettable(L, idx); }
        
        /// lua_gettable
        /// push [top-1][[top]]
        void getTable() const noexcept { lua_gettable(L, -2); }

        /// lua_getfield
        /// push [idx][k]
        void getField(Index idx, const ZStringArg auto& k) const noexcept { lua_getfield(L, idx, detail::to_cstr(k)); }

        /// lua_getfield
        /// push [top][k]
        void getField(const ZStringArg auto& k) const noexcept { lua_getfield(L, -1, detail::to_cstr(k)); }

        /// lua_getglobal
        /// push _G[k]
        void getGlobal(const ZStringArg auto& k) const noexcept { getField(GlobalsIndex, detail::to_cstr(k)); }

        /// lua_rawget
        /// push [idx][[top]]
        void rawGet(Index idx) const noexcept { lua_rawget(L, idx); }

        /// lua_rawgeti
        /// push [idx][n]
        void rawGet(Index idx, int n) const noexcept { lua_rawgeti(L, idx, n); }

        /// lua_newtable
        void newTable() const noexcept { lua_createtable(L, 0, 0); }

        /// lua_createtable
        void newTable(int narr, int nrec) const noexcept { lua_createtable(L, narr, nrec); }

        /// lua_newuserdata
        template<class T>
        [[nodiscard]] T* newUserdata() const noexcept { return static_cast<T*>(lua_newuserdata(L, sizeof T)); }

        /// lua_newuserdata
        template<class T = void>
        requires (std::same_as<T, void> || (std::is_array_v<T> && std::is_trivially_default_constructible_v<T>))
        [[nodiscard]] T* newUserdata(size_t size) const noexcept {
            if constexpr (std::same_as<T, void>) {
                return lua_newuserdata(L, size);
            }
            else {
                using T2 = std::remove_extent_t<T>;
                return static_cast<T2*>(lua_newuserdata(L, sizeof(T2) * size));
            }
        }

        /// lua_newuserdata
        template<class T, class... Args>
        requires (!(std::same_as<T, void> || (std::is_array_v<T> && std::is_trivially_default_constructible_v<T>)))
        [[nodiscard]] T* newUserdata(Args&&... args) const noexcept {
            auto ptr = static_cast<T*>(lua_newuserdata(L, sizeof(T)));
            if (!ptr) return nullptr;
            return new(ptr) T(std::forward<Args>(args)...);
        }

        /// lua_getmetatable
        /// @return メタテーブルが存在するか\n
        /// 存在しなければスタックは編集しない
        bool getMetatable(Index idx) const noexcept { return lua_getmetatable(L, idx); }

        /// luaL_getmetafield
        /// push [obj].meta[e]
        /// @return メタテーブルにフィールド k が存在するか\n
        /// 存在しなければスタックは編集しない
        bool getMetaField(Index idx, const ZStringArg auto& k) const noexcept { return luaL_getmetafield(L, idx, detail::to_cstr(k)); }

        /// luaL_getmetafield
        /// push [obj].meta[e]
        /// @return メタテーブルにフィールド k が存在するか\n
        /// 存在しなければスタックは編集しない
        bool getMetaField(const ZStringArg auto& k) const noexcept { return getMetaField(-1, detail::to_cstr(k)); }


        /// luaL_callmeta
        /// push ([obj].meta[e]([obj]))
        /// @return メタテーブルにフィールド k が存在するか\n
        /// 存在しなければスタックは編集しない
        /// @warning この関数はLuaのエラーを送出することがある
        bool callMeta(Index idx, const ZStringArg auto& k) const noexcept { return luaL_callmeta(L, idx, detail::to_cstr(k)); }

        // lua_getfenv
        void getFEnv(Index idx) const noexcept { lua_getfenv(L, idx); }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

        /// lua_settable
        /// ST[idx][ST[top-1]]=ST[top]; pop(2)
        void setTable(Index idx) const noexcept { lua_settable(L, idx); }

        /// lua_settable
        void setTable(Index idx, auto&& key, auto&& value) const noexcept {
            const auto idxa = isAbsoluteIndex(idx) ? idx : idx - 2;
            push(std::forward<decltype(key)>(key));
            push(std::forward<decltype(value)>(value));
            lua_settable(L, idxa);
        }

        /// lua_setfield
        void setField(Index idx, const ZStringArg auto& k) const noexcept { lua_setfield(L, idx, detail::to_cstr(k)); }

        // ST[top][key] = value
        template<ZStringArg KeyT, class ValT>
        requires requires(State* L, Index i, KeyT k, ValT v) {
            L->push(v);
            L->setField(i, k);
        }
        void emplaceField(KeyT key, ValT value) const noexcept {
            this->push(value);
            this->setField(-2, key);
        }

        /// lua_setglobal
        void setGlobal(const ZStringArg auto& k) const noexcept { setField(GlobalsIndex, detail::to_cstr(k)); }

        /// lua_rawset
        void rawSet(Index idx) const noexcept { lua_rawset(L, idx); }

        /// lua_rawseti
        void rawSet(Index idx, int n) const noexcept { lua_rawseti(L, idx, n); }

        /// lua_setmetatable
        void setMetatable(Index idx) const noexcept { lua_setmetatable(L, idx); }

        /// lua_setfenv
        bool setFEnv(Index idx) const noexcept { return lua_setfenv(L, idx); }

        /// lua_register
        void registerLib(const ZStringArg auto& name, CFunction f) const noexcept {
            pushCFunction(f);
            setGlobal(detail::to_cstr(name));
        }
        
        /// luaL_register
        /// @param r range of Reg
        void registerLib(RegRange auto&& r) const noexcept {
            for (const auto& e : r) {
                pushCFunction(e.func);
                setField(-2, e.name);
            }
        }

        /*
        * t = package.loaded[libname]
        * if (!isTable(t)) {
        *   t = _G[libname]
        *   if (!isTable(t)) {
        *     t = {}
        *   }
        * }
        */
        void registerLib(const ZStringArg auto& libname, RegRange auto&& r) const {
            const auto libname_p = detail::to_cstr(libname);
            getGlobal("package");
            getField("loaded");
            remove(-2);
            getField(libname_p);
            if (!isTable(-1)) {
                pop(1);
                getGlobal(libname_p);

                switch (type(-1)) {
                    case Type::Table:
                        break;
                    case Type::Nil:
                        pop(1);
                        newTable(0, std::ranges::size(r));
                        break;
                    default:
                        throw Exception{ "Name conflict for module '{}'", libname_p };
                }
                
                pushValue(-1);
                setField(-3, libname_p);
                
                pushValue(-1);
                setGlobal(libname_p);
            }
            replace(-2);
            registerLib(std::forward<decltype(r)>(r));
        }


// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

        /// lua_call
        void call(int nargs, int nresults) const noexcept { lua_call(L, nargs, nresults); }

        /// lua_call
        int call(int nargs) const noexcept {
            const auto top = getTop();
            lua_call(L, nargs, MultRet);
            return getTop() - top + (nargs + 1);
        }

        /// lua_pcall
        [[nodiscard]] Status pCall(int nargs, int nresults, IndexAbsolute errfunc) const noexcept {
            return Status{ lua_pcall(L, nargs, nresults, errfunc) };
        }

        /// lua_pcall
        /// errfuncには疑似インデックスも使える
        /// @return 戻り値の数、またはエラーステータス
        template<int nargs, Index errfunc>
        [[nodiscard]] std::expected<int, Status> pCall() const noexcept {
            if constexpr (isAbsoluteIndex(errfunc)) {
                const auto top = getTop();
                if (const auto s = pCall(nargs, MultRet, errfunc); s != Status::Ok) return std::unexpected{s};
                return getTop() - top + (nargs + 1);
            }
            else {
                const auto top = getTop();
                const auto err_aidx = getAbsoluteIndex(errfunc, top);
                if (const auto s = pCall(nargs, MultRet, err_aidx); s != Status::Ok) return std::unexpected{s};
                return getTop() - top + (nargs + 1);
            }
        }

        template<int nargs>
        [[nodiscard]] std::expected<int, Status> pCall(CFunction errfunc) const noexcept {
            const auto top = getTop();
            const auto call_base = top - nargs;
            push(errfunc);
            insert(call_base);
            if (const auto s = pCall(nargs, MultRet, call_base); s != Status::Ok) return std::unexpected{s};
            remove(call_base);
            return getTop() - call_base + 1;
        }


        // lua_cpcall
        [[nodiscard]] Status cpCall(CFunction func, void* ud) const noexcept { return Status{ lua_cpcall(L, func, ud) }; }

        /// lua_load
        [[nodiscard]] Status load(Reader reader, void* data, const ZStringArg auto& chunkname) const noexcept { return Status{ lua_load(L, reader, data, detail::to_cstr(chunkname)) }; }

        /// lua_load
        template<class T>
        [[nodiscard]] Status load(loadInvocable auto&& reader, const ZStringArg auto& chunkname) const noexcept {
            using ReaderT = std::remove_reference_t<decltype(reader)>;
            return Status{ lua_load(L, [](lua_State* L, void* reader_p, size_t* sz) -> int {
                std::span<const char> ret = std::invoke_r<std::span<const char>>(*static_cast<ReaderT*>(reader_p), State{L});
                *sz = ret.size_bytes();
                return ret.data();
            }, &reader, detail::to_cstr(chunkname)) };
        }

        /// lua_dump
        [[nodiscard]] int dump(Writer writer, void* data) const noexcept { return lua_dump(L, writer, data); }

        /// lua_dump
        [[nodiscard]] int dump(dumpInvocable auto&& writer) const noexcept {
            using DumperT = std::remove_reference_t<decltype(writer)>;
            return lua_dump(L, [](lua_State* L, const void* p, size_t sz, void* writer_p) -> int {
                return std::invoke_r<int>(*static_cast<DumperT*>(writer_p), State{L}, std::span<const std::byte>(p, sz));
            }, &writer);
        }

        // luaL_loadbuffer
        int loadBuffer(std::span<const char> buff, const ZStringArg auto& name) const noexcept { return luaL_loadbuffer(L, buff.data(), buff.size_bytes(), detail::to_cstr(name)); }

        /// luaL_loadfile
        Status loadFile(const ZStringArg auto& filename) const noexcept { return Status{ luaL_loadfile(L, detail::to_cstr(filename)) }; }

        /// luaL_loadfile
        Status loadFile(const std::filesystem::path& filename) const noexcept { return Status{ luaL_loadfile(L, detail::to_cstr(filename.string())) }; }

        // luaL_loadstring
        Status loadString(const ZStringArg auto& s) const noexcept { return Status{ luaL_loadstring(L, detail::to_cstr(s)) }; }
        

        /// 詳細なエラーを返す
        std::expected<int, Status> doFile(const PathArg auto& filename) const noexcept {
            if (const auto s = loadFile(filename); s != Status::Ok) return std::unexpected{s};
            return pCall<0, 0>();
        }


        /// 詳細なエラーを返す
        std::expected<int, Status> doString(const ZStringArg auto& str) const noexcept {
            if (const auto s = loadString(detail::to_cstr(str)); s != Status::Ok) return std::unexpected{s};
            return pCall<0, 0>();
        }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

        // lua_yield
        [[nodiscard]] int yield(int nresults) const noexcept { return lua_yield(L, nresults); }

        // lua_resume
        [[nodiscard]] Status resume(int narg) const noexcept { return Status{ lua_resume(L, narg) }; }

        // lua_status
        [[nodiscard]] Status status() const noexcept { return Status{ lua_status(L) }; }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

        /// lua_gc
        int gc(int what, int data) const noexcept { return lua_gc(L, what, data); }
        void gcStop() const noexcept { gc(LUA_GCSTOP, NULL); }
        void gcRestart() const noexcept { gc(LUA_GCRESTART, NULL); }
        void gcCollect() const noexcept { gc(LUA_GCCOLLECT, NULL); }
        [[nodiscard]] int gcCount() const noexcept { return gc(LUA_GCCOUNT, NULL); }
        [[nodiscard]] int gcCountB() const noexcept { return gc(LUA_GCCOUNTB, NULL); }
        [[nodiscard]] int gcStep(int data) const noexcept { return gc(LUA_GCSTEP, data); }
        int gcSetPause(int data) const noexcept { return gc(LUA_GCSETPAUSE, data); }
        int gcSetStepMul(int data) const noexcept { return gc(LUA_GCSETSTEPMUL, data); }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

        /// lua_error
        [[noreturn]] void error() const noexcept {
            lua_error(L);
            std::unreachable();
        }

        // luaL_where
        void where(int lvl) const noexcept { luaL_where(L, lvl); }

        // luaL_typerror
        // "{tname} expected, got {typeName(narg)}"
        [[noreturn]] void throwTypeException(IndexAbsolute narg, std::string_view tname) const {
            throw TypeException{ narg, tname, typeName(narg) };
        }

        template<Type t>
        [[noreturn]] void throwTypeException(IndexAbsolute narg) const {
            constexpr auto tname = typeName<t>();
            throw TypeException{ narg, tname, typeName(narg) };
        }


        /// lua_next
        [[nodiscard]] bool next(Index idx) const noexcept { return lua_next(L, idx); }

        /// lua_concat
        void concat(int n) const noexcept { lua_concat(L, n); }

        // lua_getallocf
        [[nodiscard]] Alloc getAllocF(void*& ud) const noexcept { return lua_getallocf(L, &ud); }

        // lua_setallocf
        void setAllocF(Alloc f, void* ud) const noexcept { lua_setallocf(L, f, ud); }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

        // lua_setlevel
        void setLevel(State dst) const noexcept { lua_setlevel(L, dst.getRawState()); }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

        // lua_getstack
        int getStack(int level, Debug& ar) const noexcept { return lua_getstack(L, level, reinterpret_cast<lua_Debug*>(&ar)); }

        // lua_getinfo
        [[nodiscard]] std::optional<Debug> getInfo(const char* what) const noexcept {
            Debug d;
            auto ret = lua_getinfo(L, what, reinterpret_cast<lua_Debug*>(&d));
            if (ret == 0) return std::nullopt;
            return d;
        }

        // lua_getinfo
        int getInfo(const char* what, Debug& ar) const noexcept { return lua_getinfo(L, what, reinterpret_cast<lua_Debug*>(&ar)); }

        // lua_getlocal
        [[nodiscard]] const char* getLocal(const Debug& ar, int n) const noexcept { return lua_getlocal(L, reinterpret_cast<const lua_Debug*>(&ar), n); }

        // lua_setlocal
        [[nodiscard]] const char* setLocal(const Debug& ar, int n) const noexcept { return lua_setlocal(L, reinterpret_cast<const lua_Debug*>(&ar), n); }

        // lua_getupvalue
        const char* getUpvalue(Index funcindex, int n) const noexcept { return lua_getupvalue(L, funcindex, n); }

        // lua_setupvalue
        const char* setUpvalue(Index funcindex, int n) const noexcept { return lua_setupvalue(L, funcindex, n); }

        // lua_sethook
        int setHook(Hook func, EventMask mask, int count) const noexcept { return lua_sethook(L, func, static_cast<int>(mask), count); }

        // lua_gethook
        [[nodiscard]] Hook getHook() const noexcept { return lua_gethook(L); }

        // lua_gethookmask
        [[nodiscard]] int getHookMask() const noexcept { return lua_gethookmask(L); }

        // lua_gethookcount
        [[nodiscard]] int getHookCount() const noexcept { return lua_gethookcount(L); }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

        inline static constexpr const char* LuaClassRegName = "luaClassReg";

        /// luaL_newmetatable
        bool newRegMetatable(const ZStringArg auto& tname) const noexcept {
            return luaL_newmetatable(L, detail::to_cstr(tname));
        }


        /// luaClassとして型を登録する
        /// スタックにこのクラスへ紐づいたメタテーブルが積まれて返る
        /// @return メタテーブルの構築を行ったか(2回目以降は既に作ったものを使う)
        template<class T>
        bool newRegClassMetatable() const noexcept {
            newRegMetatable(LuaClassRegName);
            const auto name = luaClassName<T>(*this);
            getField(name);
            auto is_nil = isNil(-1);
            if (is_nil) {
                pop(1);
                newTable();
                if constexpr (!std::is_trivially_destructible_v<T>) {
                    emplaceField("__gc", &luaClassDtorDefault<T>);
                }
                luaClassRequire<T>(*this);
                pushValue(-1);
                setField(-3, name);
            }
            replace(-2);

            return is_nil;
        }

        /// luaL_getmetafield
        void getRegMetatable(const ZStringArg auto& tname) const noexcept {
            getField(RegistryIndex, detail::to_cstr(tname));
        }

        /// スタックにこのクラスへ紐づいたメタテーブルが積まれて返る
        /// 初期化を完了していなければスタックは何もせずfalseを返す
        template<class T>
        [[nodiscard]] bool getRegClassMetatable() const noexcept {
            newRegMetatable(LuaClassRegName);
            const auto name = luaClassName<T>(*this);
            getField(name);
            if (isNil(-1)) {
                pop(2);
                return false;
            }
            remove(-2);
            return true;
        }

        /// luaL_checkudata
        template<class T = void>
        [[nodiscard]] auto checkUdata(IndexAbsolute narg, const ZStringArg auto& tname) const noexcept -> std::add_pointer_t<T> {
            const auto tname_p = detail::to_cstr(tname);
            auto ptr = toUserdata<T>(narg);
            if (ptr == nullptr) {
                throwTypeException(narg, tname_p);
            }
            if (!getMetatable(narg)) {
                throwTypeException(narg, tname_p);
            }
            getRegMetatable(tname);
            const auto meta_ok = rawEqual(-2, -1);
            pop(2);
            if (!meta_ok) {
                throwTypeException(narg, tname_p);
            }
            return ptr;
        }

        // lua_newuserdata (with C++ class construct)
        template<class T, class... Args>
        requires std::is_constructible_v<T, Args...>
        [[nodiscard]] auto newClass(Args&&... args) const -> std::reference_wrapper<T> {
            auto ptr = static_cast<T*>(newUserdata(sizeof(T)));
            if (!getRegClassMetatable<T>()) {
                throw Exception{ "Attempt to construct unregistered luaClass {}", Lua::luaClassName<T>(*this) };
            }
            auto ret = std::construct_at(ptr, std::forward<Args>(args)...);
            setMetatable(-2);
            return std::ref(*ret);
        }
        
        /// luaClassでなければnullptrを返す
        template<class T>
        [[nodiscard]] auto toClass(Index idx) const noexcept -> std::add_pointer_t<T> {
            if (!getMetatable(idx)) return nullptr;
            if (!getRegClassMetatable<T>()) {
                pop(1);
                return nullptr;
            }
            auto meta_ok = rawEqual(-2, -1);
            pop(2);
            return meta_ok ? toUserdata<T>(idx) : nullptr;
        }

        /// luaClassを要求する
        template<class T>
        [[nodiscard]] auto checkClass(IndexAbsolute idx) const noexcept -> std::reference_wrapper<T> {
            const auto name = luaClassName<T>(*this);
            if (!getMetatable(idx)) {
                throwTypeException(idx, name);
            }
            if (!getRegClassMetatable<T>()) {
                pop(1);
                throwTypeException(idx, name);
            }

            auto meta_ok = rawEqual(-2, -1);
            pop(2);
            if (!meta_ok) {
                throwTypeException(idx, name);
            }
            
            if (!isUserdata(idx)) {
                throwTypeException(idx, name);
            }
            return std::ref(*toUserdata<T>(idx));
        }

        // luaL_ref
        int ref(int t) const noexcept { return luaL_ref(L, t); }

        // luaL_unref
        void unref(int t, int ref) const noexcept { luaL_unref(L, t, ref); }

        // lua_ref
        int ref(bool b) const {
            if (!b) {
                throw Exception{ "unlocked references are obsolete" };
            }
            return luaL_ref(L, RegistryIndex);
        }

        // lua_unref
        void unref(int ref) const noexcept { luaL_unref(L, RegistryIndex, ref); }

        // lua_getref
        void getref(int ref) const noexcept { lua_rawgeti(L, RegistryIndex, ref); }

        // luaL_gsub
        const char* gsub(const ZStringArg auto& s, const ZStringArg auto& p, const ZStringArg auto& r) const noexcept { return luaL_gsub(L, detail::to_cstr(s), detail::to_cstr(p) ,detail::to_cstr(r)); }

        // luaL_findtable
        const char* findTable(Index idx, const char* fname, int szhint) const noexcept { return luaL_findtable(L, idx, fname, szhint); }
    };

    class StackIndex {
        State L_;
        int idx_;
    public:
        constexpr StackIndex(lua_State* L, int idx) noexcept : L_(L), idx_(idx) {}

        constexpr State& getState() noexcept { return L_; }
        constexpr State getState() const noexcept { return L_; }

        [[nodiscard]] Type type() const noexcept { return L_.type(idx_); }
        [[nodiscard]] const char* typeName() const noexcept { return L_.typeName(idx_); }
        [[nodiscard]] size_t objLen() const noexcept { return L_.objLen(idx_); }

        [[nodiscard]] bool isNone() const noexcept { return L_.isNone(idx_); }
        [[nodiscard]] bool isNil() const noexcept { return L_.isNil(idx_); }
        [[nodiscard]] bool isNoneOrNil() const noexcept { return L_.isNoneOrNil(idx_); }

        [[nodiscard]] bool isBoolean() const noexcept { return L_.isBoolean(idx_); }

        [[nodiscard]] bool isUserdata() const noexcept { return L_.isUserdata(idx_); }
        [[nodiscard]] bool isLightUserdata() const noexcept { return L_.isLightUserdata(idx_); }
        [[nodiscard]] bool isFullUserdata() const noexcept { return L_.isFullUserdata(idx_); }

        [[nodiscard]] bool isNumber() const noexcept { return L_.isNumber(idx_); }
        [[nodiscard]] bool isNumberConvertable() const noexcept { return L_.isNumberConvertable(idx_); }

        [[nodiscard]] bool isString() const noexcept { return L_.isString(idx_); }
        [[nodiscard]] bool isStringOrNumber() const noexcept { return L_.isStringOrNumber(idx_); }

        [[nodiscard]] bool isTable() const noexcept { return L_.isTable(idx_); }

        [[nodiscard]] bool isFunction() const noexcept { return L_.isFunction(idx_); }
        [[nodiscard]] bool isCFunction() const noexcept { return L_.isCFunction(idx_); }
        [[nodiscard]] bool isLFunction() const noexcept { return L_.isLFunction(idx_); }

        [[nodiscard]] bool isThread() const noexcept { return L_.isThread(idx_); }


        [[nodiscard]] auto toBoolean() const noexcept { return L_.toBoolean(idx_); }

        [[nodiscard]] auto toNumber() const noexcept { return L_.toNumber(idx_); }
        [[nodiscard]] auto tryToNumber() const noexcept { return L_.tryToNumber(idx_); }
        [[nodiscard]] auto checkNumber() const { return L_.checkNumber(idx_); }
        [[nodiscard]] auto optNumber() const { return L_.optNumber(idx_); }
        [[nodiscard]] auto optNumber(Number d) const {  return L_.optNumber(idx_, d); }

        [[nodiscard]] auto toInteger() const noexcept { return L_.toInteger(idx_); }
        [[nodiscard]] auto tryToInteger() const noexcept { return L_.tryToInteger(idx_); }
        [[nodiscard]] auto checkInteger() const { return L_.checkInteger(idx_); }
        [[nodiscard]] auto optInteger() const { return L_.optInteger(idx_); }
        [[nodiscard]] auto optInteger(Integer d) const {  return L_.optInteger(idx_, d); }

        [[nodiscard]] auto toString() const noexcept { return L_.toString(idx_); }
        [[nodiscard]] auto toStringTrusted() const noexcept { return L_.toStringTrusted(idx_); }
        [[nodiscard]] auto toStringSafe() const noexcept { return L_.toStringSafe(idx_); }
        [[nodiscard]] auto toStringStrict() const noexcept { return L_.toStringStrict(idx_); }
        [[nodiscard]] auto checkString() const { return L_.checkString(idx_); }
        [[nodiscard]] auto checkStringSafe() const { return L_.checkStringSafe(idx_); }
        [[nodiscard]] auto checkStringStrict() const { return L_.checkStringStrict(idx_); }
        [[nodiscard]] auto optString() const { return L_.optString(idx_); }
        [[nodiscard]] auto optStringSafe() const { return L_.optStringSafe(idx_); }
        [[nodiscard]] auto optStringSafe(const StringArg auto& dephault) const { return L_.optStringSafe(idx_, dephault); }
        [[nodiscard]] auto optStringStrict() const { return L_.optStringStrict(idx_); }

        [[nodiscard]] auto toCFunction() const noexcept { return L_.toCFunction(idx_); }
        template<class T = void>
        [[nodiscard]] auto toUserdata() const noexcept { return L_.toUserdata<T>(idx_); }
        [[nodiscard]] auto toThread() const noexcept { return L_.toThread(idx_); }
        [[nodiscard]] auto toPointer() const noexcept { return L_.toPointer(idx_); }

        template<class T>
        [[nodiscard]] auto toClass() const noexcept { return L_.toClass<T>(idx_); }

        [[nodiscard]] auto checkAny() const { return L_.checkAny(idx_); }
        [[nodiscard]] auto checkType(Type t) const noexcept { return L_.checkType(idx_, t); }

        [[nodiscard]] auto checkOption(const ZStringArg auto& def, const char* const lst[]) const noexcept { return L_.checkOption(idx_, def, lst); }

        [[nodiscard]] auto checkArrayOfNumber() const { return L_.checkArrayOfNumber(idx_); }

        [[nodiscard]] auto checkArrayOfString() const { return L_.checkArrayOfString(idx_); }

        template<class T = void>
        [[nodiscard]] auto checkUdata(const ZStringArg auto& name) const noexcept { return L_.checkUdata<T>(idx_, name); }

        template<class T>
        [[nodiscard]] decltype(auto) checkClass() const noexcept { return L_.checkClass<T>(idx_); }

        [[noreturn]] void throwArgException(std::string_view mes) const {
            throw ArgException{ idx_, mes };
        }

        [[noreturn]] void throwTypeException(std::string_view expected) const {
            L_.throwTypeException(idx_, expected);
        }

        friend inline static bool operator==(const StackIndex& a, const StackIndex& b) noexcept {
            assert(a.L_ == b.L_);
            return a.L_.equal(a.idx_, b.idx_);
        }

        friend inline static bool operator<(const StackIndex& a, const StackIndex& b) noexcept {
            assert(a.L_ == b.L_);
            return a.L_.lessThan(a.idx_, b.idx_);
        }

        friend inline static bool operator>(const StackIndex& a, const StackIndex& b) noexcept {
            return b < a;
        }

        friend inline static bool operator<=(const StackIndex& a, const StackIndex& b) noexcept {
            return !(b < a);
        }

        friend inline static bool operator>=(const StackIndex& a, const StackIndex& b) noexcept {
            return b <= a;
        }

        explicit operator bool() const {
            return L_.toBoolean(idx_);
        }
    };

    inline constexpr StackIndex State::operator[](Index idx) const noexcept {
        return { L,idx };
    }

    template<class T>
    concept addBufferInvocable = std::is_invocable_r_v<size_t, T, char*>;

    /// luaL_Buffer
    /// @details RAIIに基づいたluaL_Bufferの構築を行う
    class Buffer {
        luaL_Buffer b;
    public:
        inline static constexpr size_t size = LUAL_BUFFERSIZE;

        Buffer(State s) noexcept { luaL_buffinit(s.getRawState(), &b); }
        Buffer(State* s) noexcept { luaL_buffinit(s->getRawState(), &b); }
        ~Buffer() noexcept { luaL_pushresult(&b); }

        auto& getObject() noexcept { return b; }

        // luaL_addchar
        void addChar(char c) noexcept { luaL_addchar(&b, c); }

        // luaL_addstring
        void addString(const char* s) noexcept { luaL_addstring(&b, s); }

        // luaL_addlstring
        void addString(std::string_view s) noexcept { luaL_addlstring(&b, s.data(), s.size()); }

        // luaL_addvalue
        void addValue() noexcept { luaL_addvalue(&b); }
        
        // luaL_prepbuffer, luaL_addsize
        // size_t func(char*)
        void addBuffer(addBufferInvocable auto&& func) noexcept(std::is_nothrow_invocable_r_v<size_t, decltype(func), char*>) {
            auto buffer = luaL_prepbuffer(&b);
            const auto s = std::forward<decltype(func)>(func)(buffer);
            assert(s <= size);
            luaL_addsize(&b, size);
        }

    };

    namespace detail{} // private namespace
}

static_assert(std::is_trivially_destructible_v<Lua::State>);

/**
 * @file luawrap.hpp
 * # 実装予定の無い関数
 * これらの関数は、State::getRawState()で返る lua_State* で生のAPIを用いて呼び出せば使えるが、
 * このライブラリを使える程度に新しいC++において使う意味が無いため、ラッパーの実装は行わない。
 * ## lua_pushfstring, lua_pushvfstring
 * `std::format` がある
 * ## lua_pushliteral
 * `std::string_view` にして `State::pushStirng` を呼ぶ
 * ## luaL_opt
 * 全ての場合の / opt_* /を用意している
 * ## luaL_checkint, luaL_optint, luaL_checklong, luaL_optlong
 * `State::checkInteger`,`State::optInteger`に型引数を用意しているので、それを使う
 * 
 */
