module Bjolang.Prelude

open Bjolang.TypeChecker.TypeConstants
open Bjolang.TypeChecker

// Helper for function types
let makeFunType args ret = TFun (args, ret)

let makeVecType a = TCon("Vec", [a])

let emptyRegistry : TraitRegistry =
    { LocalTraits = Set.empty
      LocalTypes = Set.ofList ["List"; "Vec"]
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


        // I/O
        ("print", {Scheme = Scheme(["a"], [], makeFunType [TVar "a"] voidType); IsMutable = false })
        ("display", {Scheme = Scheme([], [], makeFunType [stringType] voidType); IsMutable = false })
        ("displayln", {Scheme = Scheme([], [], makeFunType [stringType] voidType); IsMutable = false })

        ("read-line", {Scheme = Scheme([], [], makeFunType [] stringType); IsMutable = false })
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

        // Vec operations
        ("vec-empty", {Scheme = Scheme(["a"], [], makeFunType [] (makeVecType (TVar "a"))); IsMutable = false })
        ("vec-get", {Scheme = Scheme(["a"], [], makeFunType [makeVecType (TVar "a"); intType] (TVar "a")); IsMutable = false })
        ("vec-set", {Scheme = Scheme(["a"], [], makeFunType [makeVecType (TVar "a"); intType; TVar "a"] (makeVecType (TVar "a"))); IsMutable = false })
        ("vec-add", {Scheme = Scheme(["a"], [], makeFunType [makeVecType (TVar "a"); TVar "a"] (makeVecType (TVar "a"))); IsMutable = false })
        ("vec-insert", {Scheme = Scheme(["a"], [], makeFunType [makeVecType (TVar "a"); intType; TVar "a"] (makeVecType (TVar "a"))); IsMutable = false })
        ("vec-remove-at", {Scheme = Scheme(["a"], [], makeFunType [makeVecType (TVar "a"); intType] (makeVecType (TVar "a"))); IsMutable = false })
        ("vec-pop", {Scheme = Scheme(["a"], [], makeFunType [makeVecType (TVar "a")] (makeVecType (TVar "a"))); IsMutable = false })
        ("vec-pop-first", {Scheme = Scheme(["a"], [], makeFunType [makeVecType (TVar "a")] (makeVecType (TVar "a"))); IsMutable = false })
        ("vec-slice", {Scheme = Scheme(["a"], [], makeFunType [makeVecType (TVar "a"); intType; intType] (makeVecType (TVar "a"))); IsMutable = false })
        ("vec-merge", {Scheme = Scheme(["a"], [], makeFunType [makeVecType (TVar "a"); makeVecType (TVar "a")] (makeVecType (TVar "a"))); IsMutable = false })
        ("vec-merge/pure", {Scheme = Scheme(["a"], [], makeFunType [makeVecType (TVar "a"); makeVecType (TVar "a")] (makeVecType (TVar "a"))); IsMutable = false })
        ("vec-split", {Scheme = Scheme(["a"], [], makeFunType [makeVecType (TVar "a"); intType] (TTuple [makeVecType (TVar "a"); makeVecType (TVar "a")])); IsMutable = false })
        ("vec-map", {Scheme = Scheme(["a"; "b"], [], makeFunType [makeVecType (TVar "a"); makeFunType [TVar "a"] (TVar "b")] (makeVecType (TVar "b"))); IsMutable = false })
        ("vec-filter", {Scheme = Scheme(["a"], [], makeFunType [makeVecType (TVar "a"); makeFunType [TVar "a"] boolType] (makeVecType (TVar "a"))); IsMutable = false })
        ("vec-fold", {Scheme = Scheme(["a"; "b"], [], makeFunType [makeVecType (TVar "a"); TVar "b"; makeFunType [TVar "b"; TVar "a"] (TVar "b")] (TVar "b")); IsMutable = false })
        ("vec-reduce", {Scheme = Scheme(["a"], [], makeFunType [makeVecType (TVar "a"); makeFunType [TVar "a"; TVar "a"] (TVar "a")] (TVar "a")); IsMutable = false })
        ("vec-for-each", {Scheme = Scheme(["a"], [], makeFunType [makeVecType (TVar "a"); makeFunType [TVar "a"] voidType] voidType); IsMutable = false })
        ("vec-for-each/range", {Scheme = Scheme(["a"], [], makeFunType [makeVecType (TVar "a"); makeFunType [TVar "a"] voidType; intType; intType] voidType); IsMutable = false })
        ("vec-iter", {Scheme = Scheme(["a"], [], makeFunType [makeVecType (TVar "a"); makeFunType [TVar "a"] boolType] boolType); IsMutable = false })
        ("vec-count", {Scheme = Scheme(["a"], [], makeFunType [makeVecType (TVar "a")] intType); IsMutable = false })
        ("vec-contains", {Scheme = Scheme(["a"], [], makeFunType [makeVecType (TVar "a"); TVar "a"] boolType); IsMutable = false })
        ("vec-compact", {Scheme = Scheme(["a"], [], makeFunType [makeVecType (TVar "a")] (makeVecType (TVar "a"))); IsMutable = false })
      ]
      Registry = emptyRegistry }