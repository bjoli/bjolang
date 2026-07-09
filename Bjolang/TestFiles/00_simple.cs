using System;
using System.Collections.Generic;

using static 00_simple;

public static class 00_simple {
    public static string match_list(List<string> l) {
        return new Func<string>(() => { List<string> _match_target_1 = l; return ((is_empty(_match_target_1) ? @false : @true) ? new Func<string>(() => { string hd = _match_target_1.Head; return new Func<string>(() => { List<string> tl = _match_target_1.Tail; return hd; })(); })() : new Func<string>(() => { List<string> Nil = _match_target_1; return "empty"; })()); })();
    }
    public static int main(T_a args) {
        return new Func<int>(() => { List<string> alst = Cons("hej", Nil); return new Func<int>(() => { void _ = displayln(match_list(alst)); return new Func<int>(() => { void _ = /* Unimplemented expression node */; return 0; })(); })(); })();
    }
}
