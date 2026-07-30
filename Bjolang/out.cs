using System;
using static BjolangRuntime;
using static _14_vec_patterns_Module;

public static class _14_vec_patterns_Module {
    public static string describe(Collections.RrbList<int> v) {
        switch (v) {
            case []: {
                return "empty";
            }
            case [var a]: {
                return "one";
            }
            case [var a, var b]: {
                return "two";
            }
            default: {
                return "many";
            }
        }
    }
    public static int sumsubrest(Collections.RrbList<int> v) {
        switch (v) {
            case [var a, .. var rest]: {
                return (a + _14_vec_patterns_Module.sumsubrest(rest));
            }
            default: {
                return 0;
            }
        }
    }
    public static string startssubzero(Collections.RrbList<int> v) {
        switch (v) {
            case [0, .. _]: {
                return "starts-with-zero";
            }
            case [var a, .. _]: {
                return "nonempty";
            }
            default: {
                return "empty";
            }
        }
    }
    public static int restsubcount(Collections.RrbList<int> v) {
        switch (v) {
            case [var a, var b, .. var rest]: {
                return vecsubcount(rest);
            }
            default: {
                return (0 - 1);
            }
        }
    }
    public static int nested(Collections.RrbList<Collections.RrbList<int>> v) {
        switch (v) {
            case [[var a, var b], .. _]: {
                return (a + b);
            }
            default: {
                return 0;
            }
        }
    }
    public static int main(int args) {
        var __vec3 = new Collections.RrbBuilder<int>();
        Collections.RrbList<int> __hoist2 = __vec3.ToImmutable();
        string __hoist1 = _14_vec_patterns_Module.describe(__hoist2);
        displayln(__hoist1);
        var __vec6 = new Collections.RrbBuilder<int>();
        __vec6.Add(7);
        Collections.RrbList<int> __hoist5 = __vec6.ToImmutable();
        string __hoist4 = _14_vec_patterns_Module.describe(__hoist5);
        displayln(__hoist4);
        var __vec9 = new Collections.RrbBuilder<int>();
        __vec9.Add(7);
        __vec9.Add(8);
        Collections.RrbList<int> __hoist8 = __vec9.ToImmutable();
        string __hoist7 = _14_vec_patterns_Module.describe(__hoist8);
        displayln(__hoist7);
        var __vec12 = new Collections.RrbBuilder<int>();
        __vec12.Add(7);
        __vec12.Add(8);
        __vec12.Add(9);
        Collections.RrbList<int> __hoist11 = __vec12.ToImmutable();
        string __hoist10 = _14_vec_patterns_Module.describe(__hoist11);
        displayln(__hoist10);
        var __vec16 = new Collections.RrbBuilder<int>();
        __vec16.Add(1);
        __vec16.Add(2);
        __vec16.Add(3);
        __vec16.Add(4);
        Collections.RrbList<int> __hoist15 = __vec16.ToImmutable();
        int __hoist14 = _14_vec_patterns_Module.sumsubrest(__hoist15);
        string __hoist13 = intsubgtstring(__hoist14);
        displayln(__hoist13);
        var __vec19 = new Collections.RrbBuilder<int>();
        Collections.RrbList<int> __hoist18 = __vec19.ToImmutable();
        string __hoist17 = _14_vec_patterns_Module.startssubzero(__hoist18);
        displayln(__hoist17);
        var __vec22 = new Collections.RrbBuilder<int>();
        __vec22.Add(0);
        __vec22.Add(9);
        Collections.RrbList<int> __hoist21 = __vec22.ToImmutable();
        string __hoist20 = _14_vec_patterns_Module.startssubzero(__hoist21);
        displayln(__hoist20);
        var __vec25 = new Collections.RrbBuilder<int>();
        __vec25.Add(5);
        __vec25.Add(9);
        Collections.RrbList<int> __hoist24 = __vec25.ToImmutable();
        string __hoist23 = _14_vec_patterns_Module.startssubzero(__hoist24);
        displayln(__hoist23);
        var __vec29 = new Collections.RrbBuilder<int>();
        __vec29.Add(1);
        __vec29.Add(2);
        __vec29.Add(3);
        __vec29.Add(4);
        __vec29.Add(5);
        Collections.RrbList<int> __hoist28 = __vec29.ToImmutable();
        int __hoist27 = _14_vec_patterns_Module.restsubcount(__hoist28);
        string __hoist26 = intsubgtstring(__hoist27);
        displayln(__hoist26);
        var __vec33 = new Collections.RrbBuilder<Collections.RrbList<int>>();
        var __vec35 = new Collections.RrbBuilder<int>();
        __vec35.Add(3);
        __vec35.Add(4);
        Collections.RrbList<int> __hoist34 = __vec35.ToImmutable();
        __vec33.Add(__hoist34);
        var __vec37 = new Collections.RrbBuilder<int>();
        __vec37.Add(5);
        __vec37.Add(6);
        Collections.RrbList<int> __hoist36 = __vec37.ToImmutable();
        __vec33.Add(__hoist36);
        Collections.RrbList<Collections.RrbList<int>> __hoist32 = __vec33.ToImmutable();
        int __hoist31 = _14_vec_patterns_Module.nested(__hoist32);
        string __hoist30 = intsubgtstring(__hoist31);
        displayln(__hoist30);
        return 0;
    }
}

public static class BjolangEntryPoint { public static void Main(string[] args) { _14_vec_patterns_Module.main(0); } }
