using System;
using static BjolangRuntime;
using static core_Module;
using static _07_named_let_Module;
using static core_Module;

public static class _07_named_let_Module {
    public static int main<T_a>(T_a args) {
        Func<uint, uint, uint> loop = default!;
        loop = (count, acc) => {
            while (true) {
                if ((count == ((uint)(0)))) {
                    return acc;
                } else {
                    var _tailArg0 = (count - ((uint)(1)));
                    var _tailArg1 = (acc * count);
                    count = _tailArg0;
                    acc = _tailArg1;
                    continue;
                }
            }
        };
        uint result = loop(100000000u, 1u);
        println(subgtstr_System_Int32.Instance, ((int)(result)));
        return 0;
    }
}

public static class BjolangEntryPoint { public static void Main(string[] args) { _07_named_let_Module.main(0); } }
