module Bjolang.Prelude

open Bjolang.TypeChecker.TypeConstants
open Bjolang.TypeChecker

// Helper for function types
let makeFunType args ret = TFun (args, ret)

let emptyRegistry : TraitRegistry =
    { LocalTraits = Set.empty
      LocalTypes = Set.singleton "List"
      Traits = Map.empty
      Implementations = Map.empty
      Aliases = Map.empty
      Records = Map.empty
      RecordFields = Map.empty }

let prelude : Env =
    { Bindings = Map.ofList [
        // Literals / Constants
        ("true", {Scheme = Scheme([], [], boolType); IsMutable = false  })
        ("false", {Scheme = Scheme([], [], boolType); IsMutable = false })

        // Math Operators (Monomorphic, restricted to Int for now to avoid overloading complexity)
        ("+", {Scheme = Scheme([], [], makeFunType [intType; intType] intType); IsMutable = false })
        ("-", {Scheme = Scheme([], [], makeFunType [intType; intType] intType); IsMutable = false })
        ("*", {Scheme = Scheme([], [], makeFunType [intType; intType] intType); IsMutable = false })
        ("/", {Scheme = Scheme([], [], makeFunType [intType; intType] intType); IsMutable = false })
        ("%", {Scheme = Scheme([], [], makeFunType [intType; intType] intType); IsMutable = false })

        // Comparison Operators
        ("=", {Scheme = Scheme([], [], makeFunType [intType; intType] boolType); IsMutable = false })
        ("<", {Scheme = Scheme([], [], makeFunType [intType; intType] boolType); IsMutable = false })
        (">", {Scheme = Scheme([], [], makeFunType [intType; intType] boolType); IsMutable = false })
        ("<=", {Scheme = Scheme([], [], makeFunType [intType; intType] boolType); IsMutable = false })
        (">=", {Scheme = Scheme([], [], makeFunType [intType; intType] boolType); IsMutable = false })

        // Polymorphic equality
        // eq? : 'a -> 'a -> bool (Pointer/Reference equality)
        ("eq?", {Scheme = Scheme(["a"], [], makeFunType [TVar "a"; TVar "a"] boolType); IsMutable = false })
        // equal? : 'a -> 'a -> bool (Structural/Generic equality)
        ("equal?", {Scheme = Scheme(["a"], [], makeFunType [TVar "a"; TVar "a"] boolType); IsMutable = false })

        // List constructors
        // Nil : List<'a>
        ("Nil", {Scheme = Scheme(["a"], [], TCon ("List", [TVar "a"])); IsMutable = false })
        // Cons : 'a -> List<'a> -> List<'a>
        ("Cons", {Scheme = Scheme(["a"], [], makeFunType [TVar "a"; TCon ("List", [TVar "a"])] (TCon ("List", [TVar "a"]))); IsMutable = false })

        // List operations
        ("is-empty", {Scheme = Scheme(["a"], [], makeFunType [TCon ("List", [TVar "a"])] boolType); IsMutable = false })
        ("head", {Scheme = Scheme(["a"], [], makeFunType [TCon ("List", [TVar "a"])] (TVar "a")); IsMutable = false })
        ("tail", {Scheme = Scheme(["a"], [], makeFunType [TCon ("List", [TVar "a"])] (TCon ("List", [TVar "a"]))); IsMutable = false })

        // I/O
        ("print", {Scheme = Scheme(["a"], [], makeFunType [TVar "a"] voidType); IsMutable = false })
        ("display", {Scheme = Scheme([], [], makeFunType [stringType] voidType); IsMutable = false })
        ("displayln", {Scheme = Scheme([], [], makeFunType [stringType] voidType); IsMutable = false })
        ("newline", {Scheme = Scheme([], [], makeFunType [] voidType); IsMutable = false })

        // String operations
        ("string-append", {Scheme = Scheme([], [], makeFunType [stringType; stringType] stringType); IsMutable = false })
        ("string-length", {Scheme = Scheme([], [], makeFunType [stringType] intType); IsMutable = false })
        ("number->string", {Scheme = Scheme([], [], makeFunType [intType] stringType); IsMutable = false })
        
        // Conversions
        ("byte->string", {Scheme = Scheme([], [], makeFunType [byteType] stringType); IsMutable = false })
        ("double->string", {Scheme = Scheme([], [], makeFunType [doubleType] stringType); IsMutable = false })
        ("long->string", {Scheme = Scheme([], [], makeFunType [longType] stringType); IsMutable = false })
        ("int->string", {Scheme = Scheme([], [], makeFunType [intType] stringType); IsMutable = false })
        ("string->int", {Scheme = Scheme([], [], makeFunType [stringType] intType); IsMutable = false })
        ("string->double", {Scheme = Scheme([], [], makeFunType [stringType] doubleType); IsMutable = false })
      ]
      Registry = emptyRegistry }