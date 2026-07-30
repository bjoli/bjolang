using System;
using static BjolangRuntime;
using static _13_naughty_kwarg_Module;

public static class _13_naughty_kwarg_Module {
    public static int f(int n, BjolangRuntime.Option<int> __kw_acc = default, BjolangRuntime.Option<int> __kw_other = default) {
        int acc;
        if (__kw_acc.IsSome) {
            acc = __kw_acc.Value;
        } else {
            int loop__1(int _i__2, int _a__3) {
                while (true) {
                    var i = _i__2;
                    var a = _a__3;
                    if ((i == 0)) {
                        return a;
                    } else {
                        var __next1 = (i - 1);
                        var __next2 = (a + 1);
                        _i__2 = __next1;
                        _a__3 = __next2;
                        continue;
                    }
                }
            }
            acc = loop__1(3, 0);
        }
        int other;
        if (__kw_other.IsSome) {
            other = __kw_other.Value;
        } else {
            other = (acc + 100);
        }
        return ((n + acc) + other);
    }
    public static void main() {
        displayln(intsubgtstring(_13_naughty_kwarg_Module.f(1)));
        return;
    }
}

public static class BjolangEntryPoint { public static void Main(string[] args) { _13_naughty_kwarg_Module.main(); } }
