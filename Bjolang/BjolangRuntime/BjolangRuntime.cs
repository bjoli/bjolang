using System.Runtime.CompilerServices;

public static class BjolangRuntime {
    
    
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static void display(object o) => Console.Write(o);
    
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static void displayln(object o) => Console.WriteLine(o);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static string readsubline() => Console.ReadLine() ?? "";

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static void newline() => Console.WriteLine();

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static IEnumerable<string> filesubreadsublinesdivseq(string path) => System.IO.File.ReadLines(path);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static string filesubreadsubtext(string path) => System.IO.File.ReadAllText(path);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static void filesubwritesubtext(string path, string contents) => System.IO.File.WriteAllText(path, contents);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static void filesubappendsubtext(string path, string contents) => System.IO.File.AppendAllText(path, contents);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static bool filesubexists_QMARK(string path) => System.IO.File.Exists(path);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static void filesubdelete(string path) => System.IO.File.Delete(path);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static string pathsubabsolute(string path) => System.IO.Path.GetFullPath(path);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static string pathsubcombine(params string[] paths) => System.IO.Path.Combine(paths);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static string pathsubdirectory(string path) => System.IO.Path.GetDirectoryName(path) ?? "";

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static string pathsubfilename(string path) => System.IO.Path.GetFileName(path);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static string pathsubfilesubextension(string path) => System.IO.Path.GetExtension(path);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static System.IO.TextReader opensubtextsubreader(string path) => System.IO.File.OpenText(path);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static System.IO.TextWriter opensubtextsubwriter(string path) => System.IO.File.CreateText(path);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static string readersubreadsubline(System.IO.TextReader reader) => reader.ReadLine() ?? "";

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static string readersubreadsubtosubend(System.IO.TextReader reader) => reader.ReadToEnd();

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static void writersubwritesubline(System.IO.TextWriter writer, string text) => writer.WriteLine(text);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static void writersubflush(System.IO.TextWriter writer) => writer.Flush();

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static void closesubhandle(IDisposable handle) => handle.Dispose();

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
    public static Collections.RrbList<T> vecsubempty<T>() where T : notnull => Collections.RrbFun.Empty<T>();

    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    public static T vecsubget<T>(Collections.RrbList<T> list, int index) where T : notnull => Collections.RrbFun.Get(list, index);

    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    public static Collections.RrbList<T> vecsubset<T>(Collections.RrbList<T> list, int index, T value) where T : notnull => Collections.RrbFun.SetItem(list, index, value);

    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    public static Collections.RrbList<T> vecsubadd<T>(Collections.RrbList<T> list, T item) where T : notnull => Collections.RrbFun.Add(list, item);

    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    public static Collections.RrbList<T> vecsubinsert<T>(Collections.RrbList<T> list, int index, T item) where T : notnull => Collections.RrbFun.Insert(list, index, item);

    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    public static Collections.RrbList<T> vecsubremovesubat<T>(Collections.RrbList<T> list, int index) where T : notnull => Collections.RrbFun.RemoveAt(list, index);

    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    public static Collections.RrbList<T> vecsubpop<T>(Collections.RrbList<T> list) where T : notnull => Collections.RrbFun.Pop(list);

    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    public static Collections.RrbList<T> vecsubpopsubfirst<T>(Collections.RrbList<T> list) where T : notnull => Collections.RrbFun.PopFirst(list);

    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    public static Collections.RrbList<T> vecsubslice<T>(Collections.RrbList<T> list, int start, int count) where T : notnull => Collections.RrbFun.Slice(list, start, count);

    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    public static Collections.RrbList<T> vecsubmerge<T>(Collections.RrbList<T> list, Collections.RrbList<T> other) where T : notnull => Collections.RrbFun.Merge(list, other, false);

    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    public static Collections.RrbList<T> vecsubmergedivpure<T>(Collections.RrbList<T> list, Collections.RrbList<T> other) where T : notnull => Collections.RrbFun.Merge(list, other, true);

    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    public static ValueTuple<Collections.RrbList<T>, Collections.RrbList<T>> vecsubsplit<T>(Collections.RrbList<T> list, int index) where T : notnull => Collections.RrbFun.Split(list, index);

    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    public static Collections.RrbList<TResult> vecsubmap<T, TResult>(Func<T, TResult> mapper, Collections.RrbList<T> list) where T : notnull where TResult : notnull => Collections.RrbFun.Map(list, mapper);

    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    public static Collections.RrbList<T> vecsubfilter<T>(Func<T, bool> predicate, Collections.RrbList<T> list) where T : notnull => Collections.RrbFun.Filter(list, predicate);

    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    public static TState vecsubfold<T, TState>(Func<TState, T, TState> func, TState seed, Collections.RrbList<T> list) where T : notnull => Collections.RrbFun.Fold(list, seed, func);

    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    public static T vecsubreduce<T>(Func<T, T, T> func, Collections.RrbList<T> list) where T : notnull => Collections.RrbFun.Reduce(list, func);

    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    public static void vecsubforsubeach<T>(Action<T> action, Collections.RrbList<T> list) where T : notnull => Collections.RrbFun.ForEach(list, action);

    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    public static void vecsubforsubeachdivrange<T>(Action<T> action, Collections.RrbList<T> list, int index, int count) where T : notnull => Collections.RrbFun.ForEach(list, action, index, count);

    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    public static bool vecsubiter<T>(Func<T, bool> action, Collections.RrbList<T> list) where T : notnull => Collections.RrbFun.Iter(list, action);

    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    public static int vecsubcount<T>(Collections.RrbList<T> list) where T : notnull => Collections.RrbFun.Count(list);

    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    public static bool vecsubcontains<T>(Collections.RrbList<T> list, T item) where T : notnull => Collections.RrbFun.Contains(list, item);

    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
    public static Collections.RrbList<T> vecsubcompact<T>(Collections.RrbList<T> list) where T : notnull => Collections.RrbFun.Compact(list);

    // --- VecBuilder Wrappers ---
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static Collections.RrbBuilder<T> vecbuildersubempty<T>() where T : notnull => Collections.RrbBuilderFun.Empty<T>();

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static Collections.RrbBuilder<T> vecsubgtvecbuilder<T>(Collections.RrbList<T> list) where T : notnull => Collections.RrbBuilderFun.FromList(list);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static Collections.RrbBuilder<T> vecbuildersubadd_BANG<T>(Collections.RrbBuilder<T> builder, T item) where T : notnull => Collections.RrbBuilderFun.Add(builder, item);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static Collections.RrbBuilder<T> vecbuildersubset_BANG<T>(Collections.RrbBuilder<T> builder, int index, T item) where T : notnull => Collections.RrbBuilderFun.SetItem(builder, index, item);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static T vecbuildersubget<T>(Collections.RrbBuilder<T> builder, int index) where T : notnull => Collections.RrbBuilderFun.Get(builder, index);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static int vecbuildersubcount<T>(Collections.RrbBuilder<T> builder) where T : notnull => Collections.RrbBuilderFun.Count(builder);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static Collections.RrbList<T> vecbuildersubgtvec<T>(Collections.RrbBuilder<T> builder) where T : notnull => Collections.RrbBuilderFun.ToImmutable(builder);

    // --- ListBuilder ---
    //
    // SchemeList has no builder of its own, and the obvious way to build one
    // front-to-back — cons each element then reverse — allocates two cells per
    // element. Buffering into a List<T> and handing the span to `Create`, which
    // builds back-to-front in one pass, allocates one cell per element plus the
    // buffer's amortized array.
    public sealed class ListBuilder<T> {
        public readonly List<T> Items = new List<T>();
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static ListBuilder<T> listbuildersubempty<T>() => new ListBuilder<T>();

    // Returns the builder rather than void, so that it threads through a loop
    // slot the same way an immutable accumulator does.
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static ListBuilder<T> listbuildersubadd_BANG<T>(ListBuilder<T> builder, T item) {
        builder.Items.Add(item);
        return builder;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static int listbuildersubcount<T>(ListBuilder<T> builder) => builder.Items.Count;

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static SchemeList.SchemeList<T> listbuildersubgtlist<T>(ListBuilder<T> builder) =>
        SchemeList.SchemeList.Create<T>(
            System.Runtime.InteropServices.CollectionsMarshal.AsSpan(builder.Items));

    // --- MapBuilder ---
    //
    // `MapBuilder` rather than `TransientMap`: it appends into a flat buffer and
    // sorts by CHAMP hash once at the end, which is the fastest bulk path, and
    // it has a public constructor. `TransientMap` is reachable only through
    // `map.ToTransient()` and its constructor zeroes `_count` even for a
    // non-empty root, so anything but `Empty.ToTransient()` builds a map whose
    // `Count` is wrong.
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static Map.MapBuilder<TK, TV> mapbuildersubempty<TK, TV>() where TK : notnull =>
        new Map.MapBuilder<TK, TV>();

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static Map.MapBuilder<TK, TV> mapbuildersubadd_BANG<TK, TV>(Map.MapBuilder<TK, TV> builder, TK key, TV value) where TK : notnull {
        builder.Add(key, value);
        return builder;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static Map.Map<TK, TV> mapbuildersubgtmap<TK, TV>(Map.MapBuilder<TK, TV> builder) where TK : notnull =>
        builder.ToImmutable();

    // --- Cursors ---
    //
    // Both collections have an allocation-free *struct* enumerator, which is
    // exactly what a loop cursor wants — but a struct held in a Bjolang binding
    // is a value, and `MoveNext` on a value that was copied into a call advances
    // the copy and not the loop. So the cursor a program holds is a small class
    // with the enumerator as a *field*: one allocation per loop entry, none per
    // element, and no boxing of the enumerator itself.
    //
    // `done?` is where the advancing happens, which the protocol allows for
    // exactly this reason: it is called once per iteration, before `current`,
    // and nothing peeks. `next` is then the identity.

    public sealed class VecCursor<T> where T : notnull {
        public Collections.RrbEnumerator<T> E;
        public VecCursor(Collections.RrbList<T> list) { E = list.GetEnumerator(); }
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static VecCursor<T> vecsubcursor<T>(Collections.RrbList<T> list) where T : notnull =>
        new VecCursor<T>(list);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static bool vecsubcursorsubdone_QMARK<T>(VecCursor<T> cursor) where T : notnull =>
        !cursor.E.MoveNext();

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static T vecsubcursorsubcurrent<T>(VecCursor<T> cursor) where T : notnull => cursor.E.Current;

    public sealed class MapCursor<TK, TV> where TK : notnull {
        public Map.MapEnumerator<TK, TV> E;
        public MapCursor(Map.Map<TK, TV> map) { E = map.GetEnumerator(); }
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static MapCursor<TK, TV> mapsubcursor<TK, TV>(Map.Map<TK, TV> map) where TK : notnull =>
        new MapCursor<TK, TV>(map);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static bool mapsubcursorsubdone_QMARK<TK, TV>(MapCursor<TK, TV> cursor) where TK : notnull =>
        !cursor.E.MoveNext();

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static ValueTuple<TK, TV> mapsubcursorsubcurrent<TK, TV>(MapCursor<TK, TV> cursor) where TK : notnull {
        var kv = cursor.E.Current;
        return new ValueTuple<TK, TV>(kv.Key, kv.Value);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static T[] makesubarray<T>(int length) => new T[length];

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static T arraysubref<T>(T[] arr, int index) => arr[index];

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static void arraysubset_BANG<T>(T[] arr, int index, T value) { arr[index] = value; }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static int arraysublength<T>(T[] arr) => arr.Length;

    /// An optional value. Originally only the carrier for an omitted keyword
    /// argument, which is why it is a struct: `default` is `None`, so an
    /// unsupplied parameter costs nothing. It is also Bjolang's `(Option %a)`,
    /// whose `Some`/`None` compile to the factories below and whose patterns
    /// compile to property patterns over `IsSome`/`Value`.
    public struct Option<T> : IEquatable<Option<T>> {
        public readonly bool IsSome;
        public readonly T Value;
        public Option(T value) { IsSome = true; Value = value; }
        public static implicit operator Option<T>(T value) => new Option<T>(value);

        /// What `Some` and `None` patterns actually test, and an `int` on
        /// purpose. Matching on `IsSome` directly would give C# two arms that
        /// between them cover a `bool`, so it would rule the generated
        /// match-failure arm unreachable and refuse to compile the switch.
        public int Tag => IsSome ? 1 : 0;

        // Without these, `equal?` on an Option would fall back to ValueType's
        // reflective structural comparison.
        public bool Equals(Option<T> other) =>
            IsSome == other.IsSome
            && (!IsSome || EqualityComparer<T>.Default.Equals(Value, other.Value));

        public override bool Equals(object? obj) => obj is Option<T> other && Equals(other);
        public override int GetHashCode() => IsSome ? HashCode.Combine(true, Value) : 0;
        public override string ToString() => IsSome ? $"(Some {Value})" : "None";
    }

    // --- Option constructors and accessors ---
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static Option<T> Some<T>(T value) => new Option<T>(value);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static Option<T> None<T>() => default;

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static bool some_QMARK<T>(Option<T> option) => option.IsSome;

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static bool none_QMARK<T>(Option<T> option) => !option.IsSome;

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static T optionsubget<T>(Option<T> option) =>
        option.IsSome ? option.Value : throw new InvalidOperationException("option-get on None");

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static T optionsubgetsubor<T>(Option<T> option, T fallback) =>
        option.IsSome ? option.Value : fallback;

    // --- Seq (IEnumerable) ---
    //
    // Every one of these that returns a sequence is itself an iterator, so it
    // does no work until its result is enumerated and it never holds more than
    // one element at a time. That is the whole point of `seq`: `(seq-head
    // (seq-map f xs))` calls `f` once, whatever the length of `xs`.

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static IEnumerable<T> seqsubempty<T>() => Array.Empty<T>();

    public static bool seqsubempty_QMARK<T>(IEnumerable<T> source) {
        foreach (var _ in source) return false;
        return true;
    }

    public static T seqsubhead<T>(IEnumerable<T> source) {
        foreach (var item in source) return item;
        throw new InvalidOperationException("seq-head of an empty sequence");
    }

    public static IEnumerable<T> seqsubtail<T>(IEnumerable<T> source) {
        var seenAny = false;
        foreach (var item in source) {
            if (!seenAny) { seenAny = true; continue; }
            yield return item;
        }
        if (!seenAny) throw new InvalidOperationException("seq-tail of an empty sequence");
    }

    public static IEnumerable<U> seqsubmap<T, U>(Func<T, U> selector, IEnumerable<T> source) {
        foreach (var item in source) yield return selector(item);
    }

    public static IEnumerable<T> seqsubfilter<T>(Func<T, bool> predicate, IEnumerable<T> source) {
        foreach (var item in source) if (predicate(item)) yield return item;
    }

    public static TState seqsubfold<T, TState>(Func<TState, T, TState> folder, TState initial, IEnumerable<T> source) {
        var acc = initial;
        foreach (var item in source) acc = folder(acc, item);
        return acc;
    }

    // The generator answers, for a given state, whether there is another
    // element and what the state after it is. `None` ends the sequence.
    public static IEnumerable<T> seqsubunfold<T, TState>(
        Func<TState, Option<ValueTuple<T, TState>>> generator,
        TState seed) {

        var state = seed;
        while (true) {
            var step = generator(state);
            if (!step.IsSome) yield break;
            yield return step.Value.Item1;
            state = step.Value.Item2;
        }
    }

    public static IEnumerable<T> seqsubtake<T>(IEnumerable<T> source, int count) {
        if (count <= 0) yield break;
        var taken = 0;
        foreach (var item in source) {
            yield return item;
            if (++taken >= count) yield break;
        }
    }

    public static IEnumerable<T> seqsubskip<T>(IEnumerable<T> source, int count) {
        var skipped = 0;
        foreach (var item in source) {
            if (skipped < count) { skipped++; continue; }
            yield return item;
        }
    }

    public static IEnumerable<T> seqsubappend<T>(IEnumerable<T> first, IEnumerable<T> second) {
        foreach (var item in first) yield return item;
        foreach (var item in second) yield return item;
    }

    public static void seqsubforsubeach<T>(Action<T> action, IEnumerable<T> source) {
        foreach (var item in source) action(item);
    }

    public static int seqsubcount<T>(IEnumerable<T> source) {
        var count = 0;
        foreach (var _ in source) count++;
        return count;
    }

    /// `start` inclusive, `stop` exclusive.
    public static IEnumerable<int> seqsubrange(int start, int stop) {
        for (var i = start; i < stop; i++) yield return i;
    }

    // --- Seq conversions ---
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static IEnumerable<T> listsubgtseq<T>(SchemeList.SchemeList<T> list) => list;

    public static SchemeList.SchemeList<T> seqsubgtlist<T>(IEnumerable<T> source) {
        // A cons list is built back to front, so the sequence has to be drained
        // first. This is the point at which a sequence stops being lazy.
        var buffer = new List<T>(source);
        var result = SchemeList.SchemeList.Empty<T>();
        for (var i = buffer.Count - 1; i >= 0; i--) result = SchemeList.SchemeList.Cons(buffer[i], result);
        return result;
    }

    public static IEnumerable<T> vecsubgtseq<T>(Collections.RrbList<T> vec) where T : notnull {
        var count = vec.Count;
        for (var i = 0; i < count; i++) yield return vec[i];
    }

    public static Collections.RrbList<T> seqsubgtvec<T>(IEnumerable<T> source) where T : notnull {
        var builder = Collections.RrbBuilderFun.Empty<T>();
        foreach (var item in source)  builder.Add(item);
        return builder.ToImmutable();
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
    public static SchemeList.SchemeList<U> listsubmap<T, U>(Func<T, U> selector, SchemeList.SchemeList<T> list) => SchemeList.SchemeList.Map(list, selector);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static SchemeList.SchemeList<T> listsubfilter<T>(Func<T, bool> predicate, SchemeList.SchemeList<T> list) => SchemeList.SchemeList.Filter(list, predicate);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static TState listsubfoldl<T, TState>(Func<TState, T, TState> folder, TState initial, SchemeList.SchemeList<T> list) => SchemeList.SchemeList.Fold(list, initial, folder);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static TState listsubfoldr<T, TState>(Func<T, TState, TState> folder, TState initial, SchemeList.SchemeList<T> list) => SchemeList.SchemeList.FoldRight(list, initial, folder);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static void listsubforsubeach<T>(Action<T> action, SchemeList.SchemeList<T> list) => SchemeList.SchemeList.ForEach(list, action);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static T listsubref<T>(SchemeList.SchemeList<T> list, int index) => SchemeList.SchemeList.Item(list, index);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static int listsubcount<T>(SchemeList.SchemeList<T> list) => SchemeList.SchemeList.Count(list);

    // --- Map (CHAMP) Wrappers ---
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static Map.Map<TK, TV> mapsubempty<TK, TV>() where TK : notnull => Map.Map<TK, TV>.Empty;

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static TV mapsubref<TK, TV>(Map.Map<TK, TV> map, TK key) where TK : notnull => map.Get(key);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static TV mapsubget<TK, TV>(Map.Map<TK, TV> map, TK key) where TK : notnull => map.Get(key);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static TV mapsubgetsubor<TK, TV>(Map.Map<TK, TV> map, TK key, TV fallback) where TK : notnull =>
        map.TryGetValue(key, out var val) ? val : fallback;

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static Option<TV> mapsubtrysubget<TK, TV>(Map.Map<TK, TV> map, TK key) where TK : notnull =>
        map.TryGetValue(key, out var val) ? new Option<TV>(val) : default;

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static Map.Map<TK, TV> mapsubset<TK, TV>(Map.Map<TK, TV> map, TK key, TV value) where TK : notnull =>
        map.Set(key, value);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static Map.Map<TK, TV> mapsubadd<TK, TV>(Map.Map<TK, TV> map, TK key, TV value) where TK : notnull =>
        map.Add(key, value);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static Map.Map<TK, TV> mapsubremove<TK, TV>(Map.Map<TK, TV> map, TK key) where TK : notnull =>
        map.Remove(key);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static bool mapsubcontains_QMARK<TK, TV>(Map.Map<TK, TV> map, TK key) where TK : notnull =>
        map.ContainsKey(key);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static bool mapsubhassubkey_QMARK<TK, TV>(Map.Map<TK, TV> map, TK key) where TK : notnull =>
        map.ContainsKey(key);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static int mapsubcount<TK, TV>(Map.Map<TK, TV> map) where TK : notnull => map.Count;

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static bool mapsubempty_QMARK<TK, TV>(Map.Map<TK, TV> map) where TK : notnull => map.IsEmpty;

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static Map.Map<TK, TV> mapsubclear<TK, TV>(Map.Map<TK, TV> map) where TK : notnull => map.Clear();

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static IEnumerable<TK> mapsubkeys<TK, TV>(Map.Map<TK, TV> map) where TK : notnull => map.Keys;

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static IEnumerable<TV> mapsubvalues<TK, TV>(Map.Map<TK, TV> map) where TK : notnull => map.Values;

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static Map.Map<TK, TV> mapsubmerge<TK, TV>(Map.Map<TK, TV> map, Map.Map<TK, TV> other) where TK : notnull =>
        map.Merge(other);

    // --- Map higher-order functions ---
    //
    // Every callback here takes the *pair*, as one argument. A Map's element is
    // its `(Tuple %k %v)` — `Iterable`'s `%elem` and `Foldable`'s `%item` for
    // `(Map %k %v)` both say so, as do `map->list`, `map->seq`,
    // `map-cursor-current` and the `#map(...)` literal. A trait signature that
    // mentions one element takes a one-argument callback, so a two-argument
    // function over a key and a value cannot be passed where one is expected:
    // the trait has one `%item`, not two.
    //
    // The pair is a `ValueTuple`, so passing it costs no allocation.
    // TODO: this should be fixed. Map should have an interface that works with ValueTuples instead of kvp so that we
    // do not have to create valuetuples

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static Map.Map<TK, TV> mapsubmergesubwith<TK, TV>(Func<ValueTuple<TK, TV, TV>, TV> resolver, Map.Map<TK, TV> map, Map.Map<TK, TV> other) where TK : notnull =>
        map.Merge(other, (k, a, b) => resolver(new ValueTuple<TK, TV, TV>(k, a, b)));

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static void mapsubforsubeach<TK, TV>(Action<ValueTuple<TK, TV>> action, Map.Map<TK, TV> map) where TK : notnull =>
        map.ForEach((k, v) => action(new ValueTuple<TK, TV>(k, v)));

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static bool mapsubiter<TK, TV>(Func<ValueTuple<TK, TV>, bool> action, Map.Map<TK, TV> map) where TK : notnull =>
        map.Iter((k, v) => action(new ValueTuple<TK, TV>(k, v)));

    public static TState mapsubfold<TK, TV, TState>(Func<TState, ValueTuple<TK, TV>, TState> folder, TState initial, Map.Map<TK, TV> map) where TK : notnull {
        var state = initial;
        map.Iter((k, v) => {
            state = folder(state, new ValueTuple<TK, TV>(k, v));
            return true;
        });
        return state;
    }

    public static Map.Map<TK, TV> mapsubfilter<TK, TV>(Func<ValueTuple<TK, TV>, bool> predicate, Map.Map<TK, TV> map) where TK : notnull {
        var tmap = Map.Map<TK, TV>.Empty.ToTransient();
        map.Iter((k, v) => {
            if (predicate(new ValueTuple<TK, TV>(k, v))) {
                tmap.Set(k, v);
            }
            return true;
        });
        return tmap.ToImmutable();
    }

    // Takes the pair and returns the new *value*: the key is what the result is
    // filed under, so letting the mapper move it would make collisions this
    // function has no answer for.
    public static Map.Map<TK, TV2> mapsubmap<TK, TV, TV2>(Func<ValueTuple<TK, TV>, TV2> mapper, Map.Map<TK, TV> map) where TK : notnull {
        var tmap = Map.Map<TK, TV2>.Empty.ToTransient();
        map.Iter((k, v) => {
            tmap.Set(k, mapper(new ValueTuple<TK, TV>(k, v)));
            return true;
        });
        return tmap.ToImmutable();
    }

    // `Functor` is not `Foldable`, and this is the one place a pair will not do.
    // Its `(-> %a %b)` has to replace the element type and hand back the same
    // shape, and the only argument of `(Map %k %v)` free to move is `%v` — so a
    // functorial map over a Map sees the value, with the key riding along. There
    // is no `(Map %k %v)` whose element type is a pair the functor may replace.
    public static Map.Map<TK, TV2> mapsubmapsubvalues<TK, TV, TV2>(Func<TV, TV2> mapper, Map.Map<TK, TV> map) where TK : notnull {
        var tmap = Map.Map<TK, TV2>.Empty.ToTransient();
        map.Iter((k, v) => {
            tmap.Set(k, mapper(v));
            return true;
        });
        return tmap.ToImmutable();
    }

    // --- Map Conversions ---
    public static Map.Map<TK, TV> listsubgtmap<TK, TV>(SchemeList.SchemeList<ValueTuple<TK, TV>> list) where TK : notnull {
        var tmap = Map.Map<TK, TV>.Empty.ToTransient();
        var cur = list;
        while (!SchemeList.SchemeList.IsEmpty(cur)) {
            var head = SchemeList.SchemeList.Head(cur);
            tmap.Set(head.Item1, head.Item2);
            cur = SchemeList.SchemeList.Tail(cur);
        }
        return tmap.ToImmutable();
    }

    public static SchemeList.SchemeList<ValueTuple<TK, TV>> mapsubgtlist<TK, TV>(Map.Map<TK, TV> map) where TK : notnull {
        var pairs = new List<ValueTuple<TK, TV>>(map.Count);
        map.ForEach((k, v) => pairs.Add(new ValueTuple<TK, TV>(k, v)));
        var result = SchemeList.SchemeList.Empty<ValueTuple<TK, TV>>();
        for (int i = pairs.Count - 1; i >= 0; i--) {
            result = SchemeList.SchemeList.Cons(pairs[i], result);
        }
        return result;
    }

    public static Map.Map<TK, TV> vecsubgtmap<TK, TV>(Collections.RrbList<ValueTuple<TK, TV>> vec) where TK : notnull {
        var tmap = Map.Map<TK, TV>.Empty.ToTransient();
        int count = Collections.RrbFun.Count(vec);
        for (int i = 0; i < count; i++) {
            var item = Collections.RrbFun.Get(vec, i);
            tmap.Set(item.Item1, item.Item2);
        }
        return tmap.ToImmutable();
    }

    public static Collections.RrbList<ValueTuple<TK, TV>> mapsubgtvec<TK, TV>(Map.Map<TK, TV> map) where TK : notnull {
        var builder = Collections.RrbBuilderFun.Empty<ValueTuple<TK, TV>>();
        map.ForEach((k, v) => {
            builder = Collections.RrbBuilderFun.Add(builder, new ValueTuple<TK, TV>(k, v));
        });
        return Collections.RrbBuilderFun.ToImmutable(builder);
    }

    public static Map.Map<TK, TV> seqsubgtmap<TK, TV>(IEnumerable<ValueTuple<TK, TV>> source) where TK : notnull {
        var tmap = Map.Map<TK, TV>.Empty.ToTransient();
        foreach (var (k, v) in source) {
            tmap.Set(k, v);
        }
        return tmap.ToImmutable();
    }

    public static IEnumerable<ValueTuple<TK, TV>> mapsubgtseq<TK, TV>(Map.Map<TK, TV> map) where TK : notnull {
        foreach (var kvp in map) {
            yield return new ValueTuple<TK, TV>(kvp.Key, kvp.Value);
        }
    }

    /// <summary>
    /// An interned keyword. All instances of a keyword with the same name share the same reference.
    /// </summary>
    public sealed class Keyword : IEquatable<Keyword>, IComparable<Keyword> {
        private static readonly System.Collections.Concurrent.ConcurrentDictionary<string, Keyword> _table = new();

        public string Name { get; }

        private Keyword(string name) {
            Name = name;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static Keyword Intern(string name) =>
            _table.GetOrAdd(name, static n => new Keyword(n));

        public bool Equals(Keyword? other) => ReferenceEquals(this, other);
        public override bool Equals(object? obj) => ReferenceEquals(this, obj);
        public override int GetHashCode() => RuntimeHelpers.GetHashCode(this);
        public int CompareTo(Keyword? other) => string.CompareOrdinal(Name, other?.Name);
        public override string ToString() => $":{Name}";
    }

    /// <summary>
    /// An interned symbol. All instances of a symbol with the same name share the same reference.
    /// </summary>
    public sealed class Symbol : IEquatable<Symbol>, IComparable<Symbol> {
        private static readonly System.Collections.Concurrent.ConcurrentDictionary<string, Symbol> _table = new();

        public string Name { get; }

        private Symbol(string name) {
            Name = name;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static Symbol Intern(string name) =>
            _table.GetOrAdd(name, static n => new Symbol(n));

        public bool Equals(Symbol? other) => ReferenceEquals(this, other);
        public override bool Equals(object? obj) => ReferenceEquals(this, obj);
        public override int GetHashCode() => RuntimeHelpers.GetHashCode(this);
        public int CompareTo(Symbol? other) => string.CompareOrdinal(Name, other?.Name);
        public override string ToString() => Name;
    }

    // --- Keyword & Symbol helpers ---
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static string keywordsubgtstring(Keyword k) => k.Name;

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static Keyword stringsubgtkeyword(string s) => Keyword.Intern(s);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static string symbolsubgtstring(Symbol s) => s.Name;

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static Symbol stringsubgtsymbol(string s) => Symbol.Intern(s);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static bool keyword_QMARK(object? o) => o is Keyword;

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static bool symbol_QMARK(object? o) => o is Symbol;
}

