module Bjolang.Prelude

open Bjolang.TypedAST.TypeConstants
open Bjolang.TypedAST

// Helper for function types
let makeFunType args ret = TFun (args, ret)

let makeVecType a = TCon("Vec", [a])
let makeVecBuilderType a = TCon("VecBuilder", [a])
let makeListType a = TCon("List", [a])
let makeSeqType a = TCon("Seq", [a])
let makeOptionType a = TCon("Option", [a])

let emptyRegistry : TraitRegistry =
    { LocalTraits = Set.empty
      LocalTypes = Set.ofList ["List"; "Vec"; "VecBuilder"; "Seq"; "Option"]
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

        // Math Operators (Polymorphic, deferring resolution to C#)
        ("+", {Scheme = Scheme(["a"], [], makeFunType [TVar "a"; TVar "a"] (TVar "a")); IsMutable = false })
        ("-", {Scheme = Scheme(["a"], [], makeFunType [TVar "a"; TVar "a"] (TVar "a")); IsMutable = false })
        ("*", {Scheme = Scheme(["a"], [], makeFunType [TVar "a"; TVar "a"] (TVar "a")); IsMutable = false })
        ("/", {Scheme = Scheme(["a"], [], makeFunType [TVar "a"; TVar "a"] (TVar "a")); IsMutable = false })
        ("%", {Scheme = Scheme(["a"], [], makeFunType [TVar "a"; TVar "a"] (TVar "a")); IsMutable = false })

        // Comparison Operators
        ("=", {Scheme = Scheme(["a"], [], makeFunType [TVar "a"; TVar "a"] boolType); IsMutable = false })
        ("<", {Scheme = Scheme(["a"], [], makeFunType [TVar "a"; TVar "a"] boolType); IsMutable = false })
        (">", {Scheme = Scheme(["a"], [], makeFunType [TVar "a"; TVar "a"] boolType); IsMutable = false })
        ("<=", {Scheme = Scheme(["a"], [], makeFunType [TVar "a"; TVar "a"] boolType); IsMutable = false })
        (">=", {Scheme = Scheme(["a"], [], makeFunType [TVar "a"; TVar "a"] boolType); IsMutable = false })

        // Polymorphic equality
        // eq? : 'a -> 'a -> bool (Pointer/Reference equality)
        ("eq?", {Scheme = Scheme(["a"], [], makeFunType [TVar "a"; TVar "a"] boolType); IsMutable = false })
        // equal? : 'a -> 'a -> bool (Structural/Generic equality)
        ("equal?", {Scheme = Scheme(["a"], [], makeFunType [TVar "a"; TVar "a"] boolType); IsMutable = false })


        // I/O
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

        // List constructors (builtins backed by SchemeList)
        ("Cons", {Scheme = Scheme(["a"], [], makeFunType [TVar "a"; makeListType (TVar "a")] (makeListType (TVar "a"))); IsMutable = false })
        ("Nil", {Scheme = Scheme(["a"], [], makeListType (TVar "a")); IsMutable = false })
        ("cons", {Scheme = Scheme(["a"], [], makeFunType [TVar "a"; makeListType (TVar "a")] (makeListType (TVar "a"))); IsMutable = false })

        // List operations
        ("list-empty", {Scheme = Scheme(["a"], [], makeFunType [] (makeListType (TVar "a"))); IsMutable = false })
        ("list-head", {Scheme = Scheme(["a"], [], makeFunType [makeListType (TVar "a")] (TVar "a")); IsMutable = false })
        ("list-tail", {Scheme = Scheme(["a"], [], makeFunType [makeListType (TVar "a")] (makeListType (TVar "a"))); IsMutable = false })
        ("list-empty?", {Scheme = Scheme(["a"], [], makeFunType [makeListType (TVar "a")] boolType); IsMutable = false })
        ("list-length", {Scheme = Scheme(["a"], [], makeFunType [makeListType (TVar "a")] intType); IsMutable = false })
        ("list-reverse", {Scheme = Scheme(["a"], [], makeFunType [makeListType (TVar "a")] (makeListType (TVar "a"))); IsMutable = false })
        ("list-map", {Scheme = Scheme(["a"; "b"], [], makeFunType [makeListType (TVar "a"); makeFunType [TVar "a"] (TVar "b")] (makeListType (TVar "b"))); IsMutable = false })
        ("list-filter", {Scheme = Scheme(["a"], [], makeFunType [makeListType (TVar "a"); makeFunType [TVar "a"] boolType] (makeListType (TVar "a"))); IsMutable = false })
        // Folds take the function first, then the identity, then the
        // collection: the two parts that describe *how* to fold stay together
        // at the call site instead of being split by the data.
        ("list-foldl", {Scheme = Scheme(["a"; "b"], [], makeFunType [makeFunType [TVar "b"; TVar "a"] (TVar "b"); TVar "b"; makeListType (TVar "a")] (TVar "b")); IsMutable = false })
        ("list-foldr", {Scheme = Scheme(["a"; "b"], [], makeFunType [makeFunType [TVar "a"; TVar "b"] (TVar "b"); TVar "b"; makeListType (TVar "a")] (TVar "b")); IsMutable = false })
        ("list-for-each", {Scheme = Scheme(["a"], [], makeFunType [makeListType (TVar "a"); makeFunType [TVar "a"] voidType] voidType); IsMutable = false })
        ("list-ref", {Scheme = Scheme(["a"], [], makeFunType [makeListType (TVar "a"); intType] (TVar "a")); IsMutable = false })
        ("list-count", {Scheme = Scheme(["a"], [], makeFunType [makeListType (TVar "a")] intType); IsMutable = false })

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
        ("vec-fold", {Scheme = Scheme(["a"; "b"], [], makeFunType [makeFunType [TVar "b"; TVar "a"] (TVar "b"); TVar "b"; makeVecType (TVar "a")] (TVar "b")); IsMutable = false })
        ("vec-reduce", {Scheme = Scheme(["a"], [], makeFunType [makeVecType (TVar "a"); makeFunType [TVar "a"; TVar "a"] (TVar "a")] (TVar "a")); IsMutable = false })
        ("vec-for-each", {Scheme = Scheme(["a"], [], makeFunType [makeVecType (TVar "a"); makeFunType [TVar "a"] voidType] voidType); IsMutable = false })
        ("vec-for-each/range", {Scheme = Scheme(["a"], [], makeFunType [makeVecType (TVar "a"); makeFunType [TVar "a"] voidType; intType; intType] voidType); IsMutable = false })
        ("vec-iter", {Scheme = Scheme(["a"], [], makeFunType [makeVecType (TVar "a"); makeFunType [TVar "a"] boolType] boolType); IsMutable = false })
        ("vec-count", {Scheme = Scheme(["a"], [], makeFunType [makeVecType (TVar "a")] intType); IsMutable = false })
        ("vec-contains", {Scheme = Scheme(["a"], [], makeFunType [makeVecType (TVar "a"); TVar "a"] boolType); IsMutable = false })
        ("vec-compact", {Scheme = Scheme(["a"], [], makeFunType [makeVecType (TVar "a")] (makeVecType (TVar "a"))); IsMutable = false })

        // Option
        ("Some", {Scheme = Scheme(["a"], [], makeFunType [TVar "a"] (makeOptionType (TVar "a"))); IsMutable = false })
        ("None", {Scheme = Scheme(["a"], [], makeOptionType (TVar "a")); IsMutable = false })
        ("some?", {Scheme = Scheme(["a"], [], makeFunType [makeOptionType (TVar "a")] boolType); IsMutable = false })
        ("none?", {Scheme = Scheme(["a"], [], makeFunType [makeOptionType (TVar "a")] boolType); IsMutable = false })
        ("option-get", {Scheme = Scheme(["a"], [], makeFunType [makeOptionType (TVar "a")] (TVar "a")); IsMutable = false })
        ("option-get-or", {Scheme = Scheme(["a"], [], makeFunType [makeOptionType (TVar "a"); TVar "a"] (TVar "a")); IsMutable = false })

        // Seq operations. A Seq is lazy: nothing below that returns one does any
        // work until the result is consumed.
        ("seq-empty", {Scheme = Scheme(["a"], [], makeFunType [] (makeSeqType (TVar "a"))); IsMutable = false })
        ("seq-empty?", {Scheme = Scheme(["a"], [], makeFunType [makeSeqType (TVar "a")] boolType); IsMutable = false })
        ("seq-head", {Scheme = Scheme(["a"], [], makeFunType [makeSeqType (TVar "a")] (TVar "a")); IsMutable = false })
        ("seq-tail", {Scheme = Scheme(["a"], [], makeFunType [makeSeqType (TVar "a")] (makeSeqType (TVar "a"))); IsMutable = false })
        ("seq-map", {Scheme = Scheme(["a"; "b"], [], makeFunType [makeSeqType (TVar "a"); makeFunType [TVar "a"] (TVar "b")] (makeSeqType (TVar "b"))); IsMutable = false })
        ("seq-filter", {Scheme = Scheme(["a"], [], makeFunType [makeSeqType (TVar "a"); makeFunType [TVar "a"] boolType] (makeSeqType (TVar "a"))); IsMutable = false })
        // Folds take the function first, then the identity, then the
        // collection, as `list-foldl` and `vec-fold` do.
        ("seq-fold", {Scheme = Scheme(["a"; "b"], [], makeFunType [makeFunType [TVar "b"; TVar "a"] (TVar "b"); TVar "b"; makeSeqType (TVar "a")] (TVar "b")); IsMutable = false })
        // The generator maps a state to the next element and the state after
        // it, or to None to stop.
        ("seq-unfold", {Scheme = Scheme(["a"; "s"], [], makeFunType [makeFunType [TVar "s"] (makeOptionType (TTuple [TVar "a"; TVar "s"])); TVar "s"] (makeSeqType (TVar "a"))); IsMutable = false })
        ("seq-take", {Scheme = Scheme(["a"], [], makeFunType [makeSeqType (TVar "a"); intType] (makeSeqType (TVar "a"))); IsMutable = false })
        ("seq-skip", {Scheme = Scheme(["a"], [], makeFunType [makeSeqType (TVar "a"); intType] (makeSeqType (TVar "a"))); IsMutable = false })
        ("seq-append", {Scheme = Scheme(["a"], [], makeFunType [makeSeqType (TVar "a"); makeSeqType (TVar "a")] (makeSeqType (TVar "a"))); IsMutable = false })
        ("seq-for-each", {Scheme = Scheme(["a"], [], makeFunType [makeSeqType (TVar "a"); makeFunType [TVar "a"] voidType] voidType); IsMutable = false })
        ("seq-count", {Scheme = Scheme(["a"], [], makeFunType [makeSeqType (TVar "a")] intType); IsMutable = false })
        ("seq-range", {Scheme = Scheme([], [], makeFunType [intType; intType] (makeSeqType intType)); IsMutable = false })

        // Seq conversions
        ("list->seq", {Scheme = Scheme(["a"], [], makeFunType [makeListType (TVar "a")] (makeSeqType (TVar "a"))); IsMutable = false })
        ("seq->list", {Scheme = Scheme(["a"], [], makeFunType [makeSeqType (TVar "a")] (makeListType (TVar "a"))); IsMutable = false })
        ("vec->seq", {Scheme = Scheme(["a"], [], makeFunType [makeVecType (TVar "a")] (makeSeqType (TVar "a"))); IsMutable = false })
        ("seq->vec", {Scheme = Scheme(["a"], [], makeFunType [makeSeqType (TVar "a")] (makeVecType (TVar "a"))); IsMutable = false })

        // VecBuilder operations
        ("vecbuilder-empty", {Scheme = Scheme(["a"], [], makeFunType [] (makeVecBuilderType (TVar "a"))); IsMutable = false })
        ("vec->vecbuilder", {Scheme = Scheme(["a"], [], makeFunType [makeVecType (TVar "a")] (makeVecBuilderType (TVar "a"))); IsMutable = false })
        ("vecbuilder-add!", {Scheme = Scheme(["a"], [], makeFunType [makeVecBuilderType (TVar "a"); TVar "a"] (makeVecBuilderType (TVar "a"))); IsMutable = false })
        ("vecbuilder-set!", {Scheme = Scheme(["a"], [], makeFunType [makeVecBuilderType (TVar "a"); intType; TVar "a"] (makeVecBuilderType (TVar "a"))); IsMutable = false })
        ("vecbuilder-get", {Scheme = Scheme(["a"], [], makeFunType [makeVecBuilderType (TVar "a"); intType] (TVar "a")); IsMutable = false })
        ("vecbuilder-count", {Scheme = Scheme(["a"], [], makeFunType [makeVecBuilderType (TVar "a")] intType); IsMutable = false })
        ("vecbuilder->vec", {Scheme = Scheme(["a"], [], makeFunType [makeVecBuilderType (TVar "a")] (makeVecType (TVar "a"))); IsMutable = false })
      ]
      Registry = emptyRegistry
      FunMetas = Map.empty }