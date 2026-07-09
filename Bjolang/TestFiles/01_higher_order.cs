using System;
using System.Collections.Generic;

using static _01_higher_order;

public static class _01_higher_order {
    public static T_a id(T_a x) {
        return x;
    }
    public static bool is_empty(List<T_a> lst) {
        return new Func<bool>(() => { List<T_a> _match_target_1 = lst; return new Func<bool>(() => { List<T_a> Nil = _match_target_1; return @true; })(); })();
    }
    public static T_a head(List<T_a> lst) {
        return new Func<T_a>(() => { List<T_a> _match_target_2 = lst; return ((is_empty(_match_target_2) ? @false : @true) ? new Func<T_a>(() => { T_a h = _match_target_2.Head; return h; })() : new Func<T_a>(() => { void _ = displayln("Match failure occurred at line 11"); return Nil; })()); })();
    }
    public static List<T_a> tail(List<T_a> lst) {
        return new Func<List<T_a>>(() => { List<T_a> _match_target_3 = lst; return ((is_empty(_match_target_3) ? @false : @true) ? new Func<List<T_a>>(() => { List<T_a> t = _match_target_3.Tail; return t; })() : new Func<List<T_a>>(() => { void _ = displayln("Match failure occurred at line 14"); return Nil; })()); })();
    }
    public static List<T_b> map(Func<T_a, T_b> f, List<T_a> lst) {
        return (is_empty(lst) ? Nil : Cons(f(head(lst)), map(f, tail(lst))));
    }
    public static Func<List<T_a>, List<T_b>> make_mapper(Func<T_a, T_b> f) {
        return (lst) => map(f, lst);
    }
    public static int main(T_a args) {
        return new Func<int>(() => { List<string> my_list = Cons("hej", Cons("då", Nil)); return new Func<int>(() => { List<T_a> my_mapper = new Func<List<T_a>>(() => { Func<List<T_a>, List<T_a>> mapper = make_mapper(id); return mapper(lst); })(); return new Func<int>(() => { List<string> h = my_mapper(my_list); return new Func<int>(() => { void _ = displayln("h generated!"); return new Func<int>(() => { void _ = displayln(head(h)); return 0; })(); })(); })(); })(); })();
    }
}
