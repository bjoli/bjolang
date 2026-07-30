using System;
using static BjolangRuntime;
using static trait_rec_Module;

public interface Countdown<T_a> {
    int runsubdown(T_a arg0, int arg1);
}
public sealed class Countdown_System_Int32 : Countdown<int> {
    public static readonly Countdown_System_Int32 Instance = new();
    public int runsubdown(int x, int acc) {
        while (true) {
            var _x__1 = x;
            var _acc__2 = acc;
            if ((_x__1 == 0)) {
                return _acc__2;
            } else {
                var __next1 = (_x__1 - 1);
                var __next2 = (_acc__2 + 1);
                x = __next1;
                acc = __next2;
                continue;
            }
        }
    }
}
public static class trait_rec_Module {
    public static void main() {
        displayln(intsubgtstring(Countdown_System_Int32.Instance.runsubdown(100000, 0)));
        return;
    }
}

public static class BjolangEntryPoint { public static void Main(string[] args) { trait_rec_Module.main(); } }
