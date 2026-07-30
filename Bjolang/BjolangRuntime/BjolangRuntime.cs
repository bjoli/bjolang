using System;
using System.Collections.Generic;
using System.Runtime.CompilerServices;

public static class BjolangRuntime {
    
    
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static void display(object o) => Console.Write(o);
    
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static void displayln(object o) => Console.WriteLine(o);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static string readsubline() => Console.ReadLine();

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static void newline() => Console.WriteLine();

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static string stringsubappend(string a, string b) => a + b;

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static int stringsublength(string s) => s.Length;

    // `number->string` is declared over `int` in the prelude, so it is the same
    // operation as `int->string`.
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static string numbersubgtstring(int i) => i.ToString();
    
    public static bool @true = true;
    public static bool @false = false;
    
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static bool eq<T>(T a, T b) => EqualityComparer<T>.Default.Equals(a, b);

    // `equal?` is structural equality; `eq?` is identity. Identity on a value
    // type would box both operands and always answer false, so it falls back to
    // structural equality there — the JIT specializes the test away per T.
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static bool equal_QMARK<T>(T a, T b) => EqualityComparer<T>.Default.Equals(a, b);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static bool eq_QMARK<T>(T a, T b) =>
        typeof(T).IsValueType ? EqualityComparer<T>.Default.Equals(a, b) : ReferenceEquals(a, b);
    
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

    // --- VecBuilder Wrappers ---
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static Collections.RrbBuilder<T> vecbuildersubempty<T>() => Collections.RrbBuilderFun.Empty<T>();

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static Collections.RrbBuilder<T> vecsubgtvecbuilder<T>(Collections.RrbList<T> list) => Collections.RrbBuilderFun.FromList(list);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static Collections.RrbBuilder<T> vecbuildersubadd_BANG<T>(Collections.RrbBuilder<T> builder, T item) => Collections.RrbBuilderFun.Add(builder, item);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static Collections.RrbBuilder<T> vecbuildersubset_BANG<T>(Collections.RrbBuilder<T> builder, int index, T item) => Collections.RrbBuilderFun.SetItem(builder, index, item);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static T vecbuildersubget<T>(Collections.RrbBuilder<T> builder, int index) => Collections.RrbBuilderFun.Get(builder, index);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static int vecbuildersubcount<T>(Collections.RrbBuilder<T> builder) => Collections.RrbBuilderFun.Count(builder);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static Collections.RrbList<T> vecbuildersubgtvec<T>(Collections.RrbBuilder<T> builder) => Collections.RrbBuilderFun.ToImmutable(builder);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static T[] makesubarray<T>(int length) => new T[length];

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static T arraysubref<T>(T[] arr, int index) => arr[index];

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static void arraysubset_BANG<T>(T[] arr, int index, T value) { arr[index] = value; }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static int arraysublength<T>(T[] arr) => arr.Length;

    public struct Option<T> {
        public readonly bool IsSome;
        public readonly T Value;
        public Option(T value) { IsSome = true; Value = value; }
        public static implicit operator Option<T>(T value) => new Option<T>(value);
    }

    // --- List (SchemeList) Wrappers ---
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static SchemeList.SchemeList<T> listsubempty<T>() => SchemeList.SchemeList.Empty<T>();

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static SchemeList.SchemeList<T> cons<T>(T car, SchemeList.SchemeList<T> cdr) => SchemeList.SchemeList.Cons(car, cdr);

    // Capital-C aliases for backward compatibility with Bjolang's Cons/Nil constructors
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static SchemeList.SchemeList<T> Cons<T>(T car, SchemeList.SchemeList<T> cdr) => SchemeList.SchemeList.Cons(car, cdr);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static SchemeList.SchemeList<T> Nil<T>() => SchemeList.SchemeList.Empty<T>();

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static T listsubhead<T>(SchemeList.SchemeList<T> list) => SchemeList.SchemeList.Head(list);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static SchemeList.SchemeList<T> listsubtail<T>(SchemeList.SchemeList<T> list) => SchemeList.SchemeList.Tail(list);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static bool listsubempty_QMARK<T>(SchemeList.SchemeList<T> list) => SchemeList.SchemeList.IsEmpty(list);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static int listsublength<T>(SchemeList.SchemeList<T> list) => SchemeList.SchemeList.Length(list);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static SchemeList.SchemeList<T> listsubreverse<T>(SchemeList.SchemeList<T> list) => SchemeList.SchemeList.Reverse(list);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static SchemeList.SchemeList<U> listsubmap<T, U>(SchemeList.SchemeList<T> list, Func<T, U> selector) => SchemeList.SchemeList.Map(list, selector);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static SchemeList.SchemeList<T> listsubfilter<T>(SchemeList.SchemeList<T> list, Func<T, bool> predicate) => SchemeList.SchemeList.Filter(list, predicate);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static TState listsubfoldl<T, TState>(SchemeList.SchemeList<T> list, TState initial, Func<TState, T, TState> folder) => SchemeList.SchemeList.Fold(list, initial, folder);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static TState listsubfoldr<T, TState>(SchemeList.SchemeList<T> list, TState initial, Func<T, TState, TState> folder) => SchemeList.SchemeList.FoldRight(list, initial, folder);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static void listsubforsubeach<T>(SchemeList.SchemeList<T> list, Action<T> action) => SchemeList.SchemeList.ForEach(list, action);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static T listsubref<T>(SchemeList.SchemeList<T> list, int index) => SchemeList.SchemeList.Item(list, index);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static int listsubcount<T>(SchemeList.SchemeList<T> list) => SchemeList.SchemeList.Count(list);
}

