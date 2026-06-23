using System;
using System.Collections.Generic;

public abstract record List<T> {
    private List() {}
    public sealed record Cons(T Item1, List<T> Item2) : List<T>;
    public sealed record Nil() : List<T>;
    
    public T Head => ((Cons)this).Item1;
    public List<T> Tail => ((Cons)this).Item2;
    
    public static implicit operator List<T>(BjolangRuntime.NilType _) => new Nil();
}

public static class BjolangRuntime {
    public struct NilType {}
    public static readonly NilType Nil = new NilType();
    
    public static List<T> Cons<T>(T head, List<T> tail) => new List<T>.Cons(head, tail);
    
    public static bool is_empty<T>(List<T> lst) => lst is List<T>.Nil || lst == null;
    
    public static void displayln(object o) => Console.WriteLine(o);
    
    public static bool @true = true;
    public static bool @false = false;
    
    public static string byte_gtstring(byte b) => b.ToString();
    public static string int_gtstring(int i) => i.ToString();
    public static string double_gtstring(double d) => d.ToString();
    public static string long_gtstring(long l) => l.ToString();
    public static int string_gtint(string s) => int.Parse(s);
    public static double string_gtdouble(string s) => double.Parse(s);
}
