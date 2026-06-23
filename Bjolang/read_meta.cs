using System;
using System.Reflection;

class Program {
    static void Main() {
        var asm = Assembly.LoadFile(System.IO.Path.GetFullPath("TestFiles/dummy_lib.dll"));
        var attrs = asm.GetCustomAttributes(typeof(AssemblyMetadataAttribute), false);
        foreach (AssemblyMetadataAttribute attr in attrs) {
            Console.WriteLine($"{attr.Key} = {attr.Value}");
        }
    }
}
