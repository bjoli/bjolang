using System;
using static BjolangRuntime;
using static list_Module;
using static iter_Module;
using static list_Module;
[assembly: System.Reflection.AssemblyMetadata("BjolangExports", "(type-rec ((List 'a)\n  (: Cons 'a (List 'a))\n  Nil))\n")]
[assembly: System.Reflection.AssemblyMetadata("BjolangDeps", "/home/linus/Programmering/Bjolang/Bjolang3/Bjolang/lib/std/list.dll")]

public interface Iterator<T_iter, T_a> {
    T_a current(T_iter arg0);
    bool done_QMARK(T_iter arg0);
    T_iter next(T_iter arg0);
}
public interface Iterable<T_col, T_iter, T_a> {
    T_iter iterate(T_col arg0);
}
public sealed class Iterator_List<T_a> : Iterator<List<T_a>, T_a> {
    public static readonly Iterator_List<T_a> Instance = new();
    public bool done_QMARK(List<T_a> lst) {
        return empty_QMARK(lst);
    }
    public T_a current(List<T_a> lst) {
        return head(lst);
    }
    public List<T_a> next(List<T_a> lst) {
        return tail(lst);
    }
}
public sealed class Iterable_List<T_a> : Iterable<List<T_a>, List<T_a>, T_a> {
    public static readonly Iterable_List<T_a> Instance = new();
    public List<T_a> iterate(List<T_a> lst) {
        return lst;
    }
}
public static class iter_Module {
}
