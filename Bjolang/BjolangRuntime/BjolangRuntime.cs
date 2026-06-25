using System;
using System.Collections.Generic;
using System.Runtime.CompilerServices;

public static class BjolangRuntime {
    
    
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static void displayln(object o) => Console.WriteLine(o);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static string readsubline() => Console.ReadLine();
    
    public static bool @true = true;
    public static bool @false = false;
    
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static bool eq<T>(T a, T b) => EqualityComparer<T>.Default.Equals(a, b);
    
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static int sub(int a, int b) => a - b;
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static int add(int a, int b) => a + b;
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static int mul(int a, int b) => a * b;
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static int div(int a, int b) => a / b;
    
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static string bytesubgtstring(byte b) => b.ToString();
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static string intsubgtstring(int i) => i.ToString();
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static string doublesubgtstring(double d) => d.ToString();
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static string longsubgtstring(long l) => l.ToString();
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static int stringsubgtint(string s) => int.Parse(s);
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static double stringsubgtdouble(string s) => double.Parse(s);

    // Vec operations mapped from RrbFun
    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    public static Collections.RrbList<T> vecsubempty<T>() => Collections.RrbFun.Empty<T>();

    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    public static T vecsubget<T>(Collections.RrbList<T> list, int index) => Collections.RrbFun.Get(list, index);

    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    public static Collections.RrbList<T> vecsubset<T>(Collections.RrbList<T> list, int index, T value) => Collections.RrbFun.SetItem(list, index, value);

    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    public static Collections.RrbList<T> vecsubadd<T>(Collections.RrbList<T> list, T item) => Collections.RrbFun.Add(list, item);

    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    public static Collections.RrbList<T> vecsubinsert<T>(Collections.RrbList<T> list, int index, T item) => Collections.RrbFun.Insert(list, index, item);

    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    public static Collections.RrbList<T> vecsubremovesubat<T>(Collections.RrbList<T> list, int index) => Collections.RrbFun.RemoveAt(list, index);

    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    public static Collections.RrbList<T> vecsubpop<T>(Collections.RrbList<T> list) => Collections.RrbFun.Pop(list);

    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    public static Collections.RrbList<T> vecsubpopsubfirst<T>(Collections.RrbList<T> list) => Collections.RrbFun.PopFirst(list);

    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    public static Collections.RrbList<T> vecsubslice<T>(Collections.RrbList<T> list, int start, int count) => Collections.RrbFun.Slice(list, start, count);

    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    public static Collections.RrbList<T> vecsubmerge<T>(Collections.RrbList<T> list, Collections.RrbList<T> other) => Collections.RrbFun.Merge(list, other, false);

    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    public static Collections.RrbList<T> vecsubmergedivpure<T>(Collections.RrbList<T> list, Collections.RrbList<T> other) => Collections.RrbFun.Merge(list, other, true);

    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    public static ValueTuple<Collections.RrbList<T>, Collections.RrbList<T>> vecsubsplit<T>(Collections.RrbList<T> list, int index) => Collections.RrbFun.Split(list, index);

    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    public static Collections.RrbList<TResult> vecsubmap<T, TResult>(Collections.RrbList<T> list, Func<T, TResult> mapper) => Collections.RrbFun.Map(list, mapper);

    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    public static Collections.RrbList<T> vecsubfilter<T>(Collections.RrbList<T> list, Func<T, bool> predicate) => Collections.RrbFun.Filter(list, predicate);

    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    public static TState vecsubfold<T, TState>(Collections.RrbList<T> list, TState seed, Func<TState, T, TState> func) => Collections.RrbFun.Fold(list, seed, func);

    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    public static T vecsubreduce<T>(Collections.RrbList<T> list, Func<T, T, T> func) => Collections.RrbFun.Reduce(list, func);

    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    public static void vecsubforsubeach<T>(Collections.RrbList<T> list, Action<T> action) => Collections.RrbFun.ForEach(list, action);

    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    public static void vecsubforsubeachdivrange<T>(Collections.RrbList<T> list, Action<T> action, int index, int count) => Collections.RrbFun.ForEach(list, action, index, count);

    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    public static bool vecsubiter<T>(Collections.RrbList<T> list, Func<T, bool> action) => Collections.RrbFun.Iter(list, action);

    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    public static int vecsubcount<T>(Collections.RrbList<T> list) => Collections.RrbFun.Count(list);

    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    public static bool vecsubcontains<T>(Collections.RrbList<T> list, T item) => Collections.RrbFun.Contains(list, item);

    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    public static Collections.RrbList<T> vecsubcompact<T>(Collections.RrbList<T> list) => Collections.RrbFun.Compact(list);
}
