using System;
using System.Reflection;

class Program {
    static void Main() {
        var asm = Assembly.LoadFrom("../BjolangRuntime/bin/Release/net10.0/Collections.dll");
        foreach (var type in asm.GetTypes()) {
            if (type.Name.Contains("Rrb")) {
                Console.WriteLine(type.FullName);
                foreach (var method in type.GetMethods(BindingFlags.Public | BindingFlags.Instance | BindingFlags.Static)) {
                    if (method.Name == "GetEnumerator" || method.Name == "MoveNext" || method.Name == "get_Current") {
                        Console.WriteLine("  " + method);
                    }
                }
            }
        }
    }
}
