module Bjolang.Prelude

open Bjolang.TypedAST.TypeConstants
open Bjolang.TypedAST

// Helper for function types
let makeFunType args ret = TFun (args, ret)

let makeVecType a = TCon("Vec", [a])
let makeVecBuilderType a = TCon("VecBuilder", [a])
let makeListBuilderType a = TCon("ListBuilder", [a])
let makeMapBuilderType k v = TCon("MapBuilder", [k; v])
let makeVecCursorType a = TCon("VecCursor", [a])
let makeMapCursorType k v = TCon("MapCursor", [k; v])
let makeListType a = TCon("List", [a])
let makeSeqType a = TCon("Seq", [a])
let makeOptionType a = TCon("Option", [a])
let makeResultType e a = TCon("Result", [e; a])
let makeMapType k v = TCon("Map", [k; v])
let makeArrayType a = TCon("Array", [a])


let emptyRegistry : TraitRegistry =
    { LocalTraits = Set.empty
      LocalTypes = Set.ofList ["List"; "Vec"; "VecBuilder"; "ListBuilder"; "MapBuilder"; "VecCursor"; "MapCursor"; "Seq"; "Option"; "Result"; "Map"; "Keyword"; "Symbol"; "Array"]
      Traits = Map.empty
      TraitMethods = Map.empty
      Implementations = Map.empty
      ImplTargets = Map.empty
      InlineMethods = Map.empty
      Aliases = Map.empty
      Records = Map.empty
      RecordFields = Map.empty
      Unions = Map.empty
      ClrClasses = Map.empty
      ClrExterns = Map.empty }

let prelude : Env =
    { Bindings = Map.ofList [
        // Literals / Constants
        "true", {Scheme = Scheme([], [], boolType); IsMutable = false  }
        "false", {Scheme = Scheme([], [], boolType); IsMutable = false }

        // Math Operators (Polymorphic, deferring resolution to C#)
        "+", {Scheme = Scheme(["a"], [], makeFunType [TVar "a"; TVar "a"] (TVar "a")); IsMutable = false }
        "-", {Scheme = Scheme(["a"], [], makeFunType [TVar "a"; TVar "a"] (TVar "a")); IsMutable = false }
        "*", {Scheme = Scheme(["a"], [], makeFunType [TVar "a"; TVar "a"] (TVar "a")); IsMutable = false }
        "/", {Scheme = Scheme(["a"], [], makeFunType [TVar "a"; TVar "a"] (TVar "a")); IsMutable = false }
        "%", {Scheme = Scheme(["a"], [], makeFunType [TVar "a"; TVar "a"] (TVar "a")); IsMutable = false }

        // Unary arithmetic, which `(- x)` and `(/ x)` desugar to.
        //
        // These exist because the obvious expansions do not typecheck: `(- x)`
        // as `(- 0 x)` unifies the literal's `int` with `x`, so negating a
        // double is a type error. A primitive keeps the operand's own type,
        // and codegen emits C#'s unary minus rather than a subtraction.
        "negate", {Scheme = Scheme(["a"], [], makeFunType [TVar "a"] (TVar "a")); IsMutable = false }
        "recip",  {Scheme = Scheme(["a"], [], makeFunType [TVar "a"] (TVar "a")); IsMutable = false }

        // Comparison Operators
        "=", {Scheme = Scheme(["a"], [], makeFunType [TVar "a"; TVar "a"] boolType); IsMutable = false }
        "<", {Scheme = Scheme(["a"], [], makeFunType [TVar "a"; TVar "a"] boolType); IsMutable = false }
        ">", {Scheme = Scheme(["a"], [], makeFunType [TVar "a"; TVar "a"] boolType); IsMutable = false }
        "<=", {Scheme = Scheme(["a"], [], makeFunType [TVar "a"; TVar "a"] boolType); IsMutable = false }
        ">=", {Scheme = Scheme(["a"], [], makeFunType [TVar "a"; TVar "a"] boolType); IsMutable = false }

        // Polymorphic equality
        // eq? : 'a -> 'a -> bool (Pointer/Reference equality)
        "eq?", {Scheme = Scheme(["a"], [], makeFunType [TVar "a"; TVar "a"] boolType); IsMutable = false }
        // equal? : 'a -> 'a -> bool (Structural/Generic equality)
        "equal?", {Scheme = Scheme(["a"], [], makeFunType [TVar "a"; TVar "a"] boolType); IsMutable = false }


        // I/O
        "display", {Scheme = Scheme([], [], makeFunType [stringType] voidType); IsMutable = false }
        "displayln", {Scheme = Scheme([], [], makeFunType [stringType] voidType); IsMutable = false }

        "read-line", {Scheme = Scheme([], [], makeFunType [] stringType); IsMutable = false }
        "newline", {Scheme = Scheme([], [], makeFunType [] voidType); IsMutable = false }

        "file-read-lines/seq", {Scheme = Scheme([], [], makeFunType [stringType] (makeSeqType stringType)); IsMutable = false }
        "file-read-text", {Scheme = Scheme([], [], makeFunType [stringType] stringType); IsMutable = false }
        "file-write-text", {Scheme = Scheme([], [], makeFunType [stringType; stringType] voidType); IsMutable = false }
        "file-append-text", {Scheme = Scheme([], [], makeFunType [stringType; stringType] voidType); IsMutable = false }
        "file-exists?", {Scheme = Scheme([], [], makeFunType [stringType] boolType); IsMutable = false }
        "file-delete", {Scheme = Scheme([], [], makeFunType [stringType] voidType); IsMutable = false }

        "path-absolute", {Scheme = Scheme([], [], makeFunType [stringType] stringType); IsMutable = false }
        "path-combine", {Scheme = Scheme([], [], makeFunType [makeArrayType stringType] stringType); IsMutable = false }
        "path-directory", {Scheme = Scheme([], [], makeFunType [stringType] stringType); IsMutable = false }
        "path-filename", {Scheme = Scheme([], [], makeFunType [stringType] stringType); IsMutable = false }
        "path-file-extension", {Scheme = Scheme([], [], makeFunType [stringType] stringType); IsMutable = false }

        "open-text-reader", {Scheme = Scheme([], [], makeFunType [stringType] (TCon("System.IO.TextReader", []))); IsMutable = false }
        "open-text-writer", {Scheme = Scheme([], [], makeFunType [stringType] (TCon("System.IO.TextWriter", []))); IsMutable = false }
        "reader-read-line", {Scheme = Scheme([], [], makeFunType [TCon("System.IO.TextReader", [])] stringType); IsMutable = false }
        "reader-read-to-end", {Scheme = Scheme([], [], makeFunType [TCon("System.IO.TextReader", [])] stringType); IsMutable = false }
        "writer-write-line", {Scheme = Scheme([], [], makeFunType [TCon("System.IO.TextWriter", []); stringType] voidType); IsMutable = false }
        "writer-flush", {Scheme = Scheme([], [], makeFunType [TCon("System.IO.TextWriter", [])] voidType); IsMutable = false }
        "close-handle", {Scheme = Scheme([], [], makeFunType [TCon("System.IDisposable", [])] voidType); IsMutable = false }

        // String operations
        "string-append", {Scheme = Scheme([], [], makeFunType [stringType; stringType] stringType); IsMutable = false }
        "string-length", {Scheme = Scheme([], [], makeFunType [stringType] intType); IsMutable = false }
        "number->string", {Scheme = Scheme([], [], makeFunType [intType] stringType); IsMutable = false }
        
        // Conversions
        "byte->string", {Scheme = Scheme([], [], makeFunType [byteType] stringType); IsMutable = false }
        "double->string", {Scheme = Scheme([], [], makeFunType [doubleType] stringType); IsMutable = false }
        "long->string", {Scheme = Scheme([], [], makeFunType [longType] stringType); IsMutable = false }
        "int->string", {Scheme = Scheme([], [], makeFunType [intType] stringType); IsMutable = false }
        "string->int", {Scheme = Scheme([], [], makeFunType [stringType] intType); IsMutable = false }
        "string->double", {Scheme = Scheme([], [], makeFunType [stringType] doubleType); IsMutable = false }

        // Keyword & Symbol conversions / predicates
        "keyword->string", {Scheme = Scheme([], [], makeFunType [keywordType] stringType); IsMutable = false }
        "string->keyword", {Scheme = Scheme([], [], makeFunType [stringType] keywordType); IsMutable = false }
        "symbol->string", {Scheme = Scheme([], [], makeFunType [symbolType] stringType); IsMutable = false }
        "string->symbol", {Scheme = Scheme([], [], makeFunType [stringType] symbolType); IsMutable = false }
        // Characters. A `char` is a Unicode scalar value, so `char->int` is a
        // codepoint rather than a UTF-16 code unit.
        //
        // No `string-ref`, and no other index-based accessor: indexing a
        // UTF-16 string by codepoint is O(n), so an innocent-looking loop over
        // indices is quadratic. String traversal belongs to a cursor.
        "char->int", {Scheme = Scheme([], [], makeFunType [charType] intType); IsMutable = false }
        "int->char", {Scheme = Scheme([], [], makeFunType [intType] charType); IsMutable = false }
        "char->string", {Scheme = Scheme([], [], makeFunType [charType] stringType); IsMutable = false }

        "keyword?", {Scheme = Scheme(["a"], [], makeFunType [TVar "a"] boolType); IsMutable = false }
        "symbol?", {Scheme = Scheme(["a"], [], makeFunType [TVar "a"] boolType); IsMutable = false }

        // List constructors (builtins backed by SchemeList)
        //
        // `list` is variadic through its `FunMeta` below, so `(list 1 2 3)`
        // spreads like any other `#:rest` function. Its *type* is the unary
        // `(-> (Array %a) (List %a))` that `#:rest` always resolves to, which is
        // what it means as a value: `(def f list)` binds the array form.
        "list", {Scheme = Scheme(["a"], [], makeFunType [makeArrayType (TVar "a")] (makeListType (TVar "a"))); IsMutable = false }
        "Cons", {Scheme = Scheme(["a"], [], makeFunType [TVar "a"; makeListType (TVar "a")] (makeListType (TVar "a"))); IsMutable = false }
        "Nil", {Scheme = Scheme(["a"], [], makeListType (TVar "a")); IsMutable = false }
        "cons", {Scheme = Scheme(["a"], [], makeFunType [TVar "a"; makeListType (TVar "a")] (makeListType (TVar "a"))); IsMutable = false }

        // List operations
        "list-empty", {Scheme = Scheme(["a"], [], makeFunType [] (makeListType (TVar "a"))); IsMutable = false }
        "list-head", {Scheme = Scheme(["a"], [], makeFunType [makeListType (TVar "a")] (TVar "a")); IsMutable = false }
        "list-tail", {Scheme = Scheme(["a"], [], makeFunType [makeListType (TVar "a")] (makeListType (TVar "a"))); IsMutable = false }
        "list-empty?", {Scheme = Scheme(["a"], [], makeFunType [makeListType (TVar "a")] boolType); IsMutable = false }
        "list-length", {Scheme = Scheme(["a"], [], makeFunType [makeListType (TVar "a")] intType); IsMutable = false }
        "list-reverse", {Scheme = Scheme(["a"], [], makeFunType [makeListType (TVar "a")] (makeListType (TVar "a"))); IsMutable = false }
        "list-map", {Scheme = Scheme(["a"; "b"], [], makeFunType [makeFunType [TVar "a"] (TVar "b"); makeListType (TVar "a")] (makeListType (TVar "b"))); IsMutable = false }
        "list-filter", {Scheme = Scheme(["a"], [], makeFunType [makeFunType [TVar "a"] boolType; makeListType (TVar "a")] (makeListType (TVar "a"))); IsMutable = false }
        // Folds take the function first, then the identity, then the
        // collection: the two parts that describe *how* to fold stay together
        // at the call site instead of being split by the data.
        "list-foldl", {Scheme = Scheme(["a"; "b"], [], makeFunType [makeFunType [TVar "b"; TVar "a"] (TVar "b"); TVar "b"; makeListType (TVar "a")] (TVar "b")); IsMutable = false }
        "list-foldr", {Scheme = Scheme(["a"; "b"], [], makeFunType [makeFunType [TVar "a"; TVar "b"] (TVar "b"); TVar "b"; makeListType (TVar "a")] (TVar "b")); IsMutable = false }
        "list-for-each", {Scheme = Scheme(["a"], [], makeFunType [makeFunType [TVar "a"] voidType; makeListType (TVar "a")] voidType); IsMutable = false }
        "list-ref", {Scheme = Scheme(["a"], [], makeFunType [makeListType (TVar "a"); intType] (TVar "a")); IsMutable = false }
        "list-count", {Scheme = Scheme(["a"], [], makeFunType [makeListType (TVar "a")] intType); IsMutable = false }

        // Vec operations
        "vec-empty", {Scheme = Scheme(["a"], [], makeFunType [] (makeVecType (TVar "a"))); IsMutable = false }
        "vec-get", {Scheme = Scheme(["a"], [], makeFunType [makeVecType (TVar "a"); intType] (TVar "a")); IsMutable = false }
        "vec-set", {Scheme = Scheme(["a"], [], makeFunType [makeVecType (TVar "a"); intType; TVar "a"] (makeVecType (TVar "a"))); IsMutable = false }
        "vec-add", {Scheme = Scheme(["a"], [], makeFunType [makeVecType (TVar "a"); TVar "a"] (makeVecType (TVar "a"))); IsMutable = false }
        "vec-insert", {Scheme = Scheme(["a"], [], makeFunType [makeVecType (TVar "a"); intType; TVar "a"] (makeVecType (TVar "a"))); IsMutable = false }
        "vec-remove-at", {Scheme = Scheme(["a"], [], makeFunType [makeVecType (TVar "a"); intType] (makeVecType (TVar "a"))); IsMutable = false }
        "vec-pop", {Scheme = Scheme(["a"], [], makeFunType [makeVecType (TVar "a")] (makeVecType (TVar "a"))); IsMutable = false }
        "vec-pop-first", {Scheme = Scheme(["a"], [], makeFunType [makeVecType (TVar "a")] (makeVecType (TVar "a"))); IsMutable = false }
        "vec-slice", {Scheme = Scheme(["a"], [], makeFunType [makeVecType (TVar "a"); intType; intType] (makeVecType (TVar "a"))); IsMutable = false }
        "vec-merge", {Scheme = Scheme(["a"], [], makeFunType [makeVecType (TVar "a"); makeVecType (TVar "a")] (makeVecType (TVar "a"))); IsMutable = false }
        "vec-merge/pure", {Scheme = Scheme(["a"], [], makeFunType [makeVecType (TVar "a"); makeVecType (TVar "a")] (makeVecType (TVar "a"))); IsMutable = false }
        "vec-split", {Scheme = Scheme(["a"], [], makeFunType [makeVecType (TVar "a"); intType] (TTuple [makeVecType (TVar "a"); makeVecType (TVar "a")])); IsMutable = false }
        "vec-map", {Scheme = Scheme(["a"; "b"], [], makeFunType [makeFunType [TVar "a"] (TVar "b"); makeVecType (TVar "a")] (makeVecType (TVar "b"))); IsMutable = false }
        "vec-filter", {Scheme = Scheme(["a"], [], makeFunType [makeFunType [TVar "a"] boolType; makeVecType (TVar "a")] (makeVecType (TVar "a"))); IsMutable = false }
        "vec-fold", {Scheme = Scheme(["a"; "b"], [], makeFunType [makeFunType [TVar "b"; TVar "a"] (TVar "b"); TVar "b"; makeVecType (TVar "a")] (TVar "b")); IsMutable = false }
        "vec-reduce", {Scheme = Scheme(["a"], [], makeFunType [makeFunType [TVar "a"; TVar "a"] (TVar "a"); makeVecType (TVar "a")] (TVar "a")); IsMutable = false }
        "vec-for-each", {Scheme = Scheme(["a"], [], makeFunType [makeFunType [TVar "a"] voidType; makeVecType (TVar "a")] voidType); IsMutable = false }
        "vec-for-each/range", {Scheme = Scheme(["a"], [], makeFunType [makeFunType [TVar "a"] voidType; makeVecType (TVar "a"); intType; intType] voidType); IsMutable = false }
        "vec-iter", {Scheme = Scheme(["a"], [], makeFunType [makeFunType [TVar "a"] boolType; makeVecType (TVar "a")] boolType); IsMutable = false }
        "vec-count", {Scheme = Scheme(["a"], [], makeFunType [makeVecType (TVar "a")] intType); IsMutable = false }
        "vec-contains", {Scheme = Scheme(["a"], [], makeFunType [makeVecType (TVar "a"); TVar "a"] boolType); IsMutable = false }
        "vec-compact", {Scheme = Scheme(["a"], [], makeFunType [makeVecType (TVar "a")] (makeVecType (TVar "a"))); IsMutable = false }

        // Array
        // Array operations
        "make-array",   { Scheme = Scheme(["a"], [], makeFunType [intType] (makeArrayType (TVar "a"))); IsMutable = false }
        "array-ref",    { Scheme = Scheme(["a"], [], makeFunType [makeArrayType (TVar "a"); intType] (TVar "a")); IsMutable = false }
        "array-set!",   { Scheme = Scheme(["a"], [], makeFunType [makeArrayType (TVar "a"); intType; TVar "a"] voidType); IsMutable = false }
        "array-length", { Scheme = Scheme(["a"], [], makeFunType [makeArrayType (TVar "a")] intType); IsMutable = false }


        // Option
        "Some", {Scheme = Scheme(["a"], [], makeFunType [TVar "a"] (makeOptionType (TVar "a"))); IsMutable = false }
        "None", {Scheme = Scheme(["a"], [], makeOptionType (TVar "a")); IsMutable = false }
        "some?", {Scheme = Scheme(["a"], [], makeFunType [makeOptionType (TVar "a")] boolType); IsMutable = false }
        "none?", {Scheme = Scheme(["a"], [], makeFunType [makeOptionType (TVar "a")] boolType); IsMutable = false }
        "option-get", {Scheme = Scheme(["a"], [], makeFunType [makeOptionType (TVar "a")] (TVar "a")); IsMutable = false }
        "option-get-or", {Scheme = Scheme(["a"], [], makeFunType [makeOptionType (TVar "a"); TVar "a"] (TVar "a")); IsMutable = false }

        // Result. Built in for the same reason Option is: a `#:exceptions`
        // interop call returns one on every invocation, so it cannot be
        // something each file has to declare for itself.
        //
        // Every one of these is shadowed by a `Result` a module declares of its
        // own — the type definition rebinds `Ok` and `Err`, and both inference
        // and code generation look at what the module declared before they look
        // here. Modules that predate this and carry their own Result keep
        // compiling to their own union, unchanged.
        "Ok", {Scheme = Scheme(["e"; "a"], [], makeFunType [TVar "a"] (makeResultType (TVar "e") (TVar "a"))); IsMutable = false }
        "Err", {Scheme = Scheme(["e"; "a"], [], makeFunType [TVar "e"] (makeResultType (TVar "e") (TVar "a"))); IsMutable = false }

        // Seq operations. A Seq is lazy: nothing below that returns one does any
        // work until the result is consumed.
        "seq-empty", {Scheme = Scheme(["a"], [], makeFunType [] (makeSeqType (TVar "a"))); IsMutable = false }
        "seq-empty?", {Scheme = Scheme(["a"], [], makeFunType [makeSeqType (TVar "a")] boolType); IsMutable = false }
        "seq-head", {Scheme = Scheme(["a"], [], makeFunType [makeSeqType (TVar "a")] (TVar "a")); IsMutable = false }
        "seq-tail", {Scheme = Scheme(["a"], [], makeFunType [makeSeqType (TVar "a")] (makeSeqType (TVar "a"))); IsMutable = false }
        "seq-map", {Scheme = Scheme(["a"; "b"], [], makeFunType [makeFunType [TVar "a"] (TVar "b"); makeSeqType (TVar "a")] (makeSeqType (TVar "b"))); IsMutable = false }
        "seq-filter", {Scheme = Scheme(["a"], [], makeFunType [makeFunType [TVar "a"] boolType; makeSeqType (TVar "a")] (makeSeqType (TVar "a"))); IsMutable = false }
        // Folds take the function first, then the identity, then the
        // collection, as `list-foldl` and `vec-fold` do.
        "seq-fold", {Scheme = Scheme(["a"; "b"], [], makeFunType [makeFunType [TVar "b"; TVar "a"] (TVar "b"); TVar "b"; makeSeqType (TVar "a")] (TVar "b")); IsMutable = false }
        // The generator maps a state to the next element and the state after
        // it, or to None to stop.
        "seq-unfold", {Scheme = Scheme(["a"; "s"], [], makeFunType [makeFunType [TVar "s"] (makeOptionType (TTuple [TVar "a"; TVar "s"])); TVar "s"] (makeSeqType (TVar "a"))); IsMutable = false }
        "seq-take", {Scheme = Scheme(["a"], [], makeFunType [makeSeqType (TVar "a"); intType] (makeSeqType (TVar "a"))); IsMutable = false }
        "seq-skip", {Scheme = Scheme(["a"], [], makeFunType [makeSeqType (TVar "a"); intType] (makeSeqType (TVar "a"))); IsMutable = false }
        "seq-append", {Scheme = Scheme(["a"], [], makeFunType [makeSeqType (TVar "a"); makeSeqType (TVar "a")] (makeSeqType (TVar "a"))); IsMutable = false }
        "seq-for-each", {Scheme = Scheme(["a"], [], makeFunType [makeFunType [TVar "a"] voidType; makeSeqType (TVar "a")] voidType); IsMutable = false }
        "seq-count", {Scheme = Scheme(["a"], [], makeFunType [makeSeqType (TVar "a")] intType); IsMutable = false }
        "seq-range", {Scheme = Scheme([], [], makeFunType [intType; intType] (makeSeqType intType)); IsMutable = false }

        // Seq conversions
        "list->seq", {Scheme = Scheme(["a"], [], makeFunType [makeListType (TVar "a")] (makeSeqType (TVar "a"))); IsMutable = false }
        "seq->list", {Scheme = Scheme(["a"], [], makeFunType [makeSeqType (TVar "a")] (makeListType (TVar "a"))); IsMutable = false }
        "vec->seq", {Scheme = Scheme(["a"], [], makeFunType [makeVecType (TVar "a")] (makeSeqType (TVar "a"))); IsMutable = false }
        "seq->vec", {Scheme = Scheme(["a"], [], makeFunType [makeSeqType (TVar "a")] (makeVecType (TVar "a"))); IsMutable = false }

        // VecBuilder operations
        "vecbuilder-empty", {Scheme = Scheme(["a"], [], makeFunType [] (makeVecBuilderType (TVar "a"))); IsMutable = false }
        "vec->vecbuilder", {Scheme = Scheme(["a"], [], makeFunType [makeVecType (TVar "a")] (makeVecBuilderType (TVar "a"))); IsMutable = false }
        "vecbuilder-add!", {Scheme = Scheme(["a"], [], makeFunType [makeVecBuilderType (TVar "a"); TVar "a"] (makeVecBuilderType (TVar "a"))); IsMutable = false }
        "vecbuilder-set!", {Scheme = Scheme(["a"], [], makeFunType [makeVecBuilderType (TVar "a"); intType; TVar "a"] (makeVecBuilderType (TVar "a"))); IsMutable = false }
        "vecbuilder-get", {Scheme = Scheme(["a"], [], makeFunType [makeVecBuilderType (TVar "a"); intType] (TVar "a")); IsMutable = false }
        "vecbuilder-count", {Scheme = Scheme(["a"], [], makeFunType [makeVecBuilderType (TVar "a")] intType); IsMutable = false }
        "vecbuilder->vec", {Scheme = Scheme(["a"], [], makeFunType [makeVecBuilderType (TVar "a")] (makeVecType (TVar "a"))); IsMutable = false }

        // SchemeListBuilder operations
        "listbuilder-empty", {Scheme = Scheme(["a"], [], makeFunType [] (makeListBuilderType (TVar "a"))); IsMutable = false }
        "list->builder", {Scheme = Scheme(["a"], [], makeFunType [makeListType (TVar "a")] (makeListBuilderType (TVar "a"))); IsMutable = false }
        "list->listbuilder", {Scheme = Scheme(["a"], [], makeFunType [makeListType (TVar "a")] (makeListBuilderType (TVar "a"))); IsMutable = false }
        "listbuilder-add!", {Scheme = Scheme(["a"], [], makeFunType [makeListBuilderType (TVar "a"); TVar "a"] (makeListBuilderType (TVar "a"))); IsMutable = false }
        "listbuilder-add-range!", {Scheme = Scheme(["a"], [], makeFunType [makeListBuilderType (TVar "a"); makeSeqType (TVar "a")] (makeListBuilderType (TVar "a"))); IsMutable = false }
        "listbuilder-count", {Scheme = Scheme(["a"], [], makeFunType [makeListBuilderType (TVar "a")] intType); IsMutable = false }
        "listbuilder->list", {Scheme = Scheme(["a"], [], makeFunType [makeListBuilderType (TVar "a")] (makeListType (TVar "a"))); IsMutable = false }

        "mapbuilder-empty", {Scheme = Scheme(["k"; "v"], [], makeFunType [] (makeMapBuilderType (TVar "k") (TVar "v"))); IsMutable = false }
        "mapbuilder-add!", {Scheme = Scheme(["k"; "v"], [], makeFunType [makeMapBuilderType (TVar "k") (TVar "v"); TVar "k"; TVar "v"] (makeMapBuilderType (TVar "k") (TVar "v"))); IsMutable = false }
        "mapbuilder->map", {Scheme = Scheme(["k"; "v"], [], makeFunType [makeMapBuilderType (TVar "k") (TVar "v")] (makeMapType (TVar "k") (TVar "v"))); IsMutable = false }

        // Cursors over the collections' native struct enumerators. `done?` is
        // what advances — the iteration protocol allows exactly that, and it is
        // what lets `next` be the identity and the traversal allocate nothing
        // after the cursor itself.
        "vec-cursor", {Scheme = Scheme(["a"], [], makeFunType [makeVecType (TVar "a")] (makeVecCursorType (TVar "a"))); IsMutable = false }
        "vec-cursor-done?", {Scheme = Scheme(["a"], [], makeFunType [makeVecCursorType (TVar "a")] boolType); IsMutable = false }
        "vec-cursor-current", {Scheme = Scheme(["a"], [], makeFunType [makeVecCursorType (TVar "a")] (TVar "a")); IsMutable = false }

        "map-cursor", {Scheme = Scheme(["k"; "v"], [], makeFunType [makeMapType (TVar "k") (TVar "v")] (makeMapCursorType (TVar "k") (TVar "v"))); IsMutable = false }
        "map-cursor-done?", {Scheme = Scheme(["k"; "v"], [], makeFunType [makeMapCursorType (TVar "k") (TVar "v")] boolType); IsMutable = false }
        "map-cursor-current", {Scheme = Scheme(["k"; "v"], [], makeFunType [makeMapCursorType (TVar "k") (TVar "v")] (TTuple [TVar "k"; TVar "v"])); IsMutable = false }

        // Map (CHAMP) operations
        "map-empty", {Scheme = Scheme(["k"; "v"], [], makeFunType [] (makeMapType (TVar "k") (TVar "v"))); IsMutable = false }
        "map-ref", {Scheme = Scheme(["k"; "v"], [], makeFunType [makeMapType (TVar "k") (TVar "v"); TVar "k"] (TVar "v")); IsMutable = false }
        "map-get", {Scheme = Scheme(["k"; "v"], [], makeFunType [makeMapType (TVar "k") (TVar "v"); TVar "k"] (TVar "v")); IsMutable = false }
        "map-get-or", {Scheme = Scheme(["k"; "v"], [], makeFunType [makeMapType (TVar "k") (TVar "v"); TVar "k"; TVar "v"] (TVar "v")); IsMutable = false }
        "map-try-get", {Scheme = Scheme(["k"; "v"], [], makeFunType [makeMapType (TVar "k") (TVar "v"); TVar "k"] (makeOptionType (TVar "v"))); IsMutable = false }
        "map-set", {Scheme = Scheme(["k"; "v"], [], makeFunType [makeMapType (TVar "k") (TVar "v"); TVar "k"; TVar "v"] (makeMapType (TVar "k") (TVar "v"))); IsMutable = false }
        "map-add", {Scheme = Scheme(["k"; "v"], [], makeFunType [makeMapType (TVar "k") (TVar "v"); TVar "k"; TVar "v"] (makeMapType (TVar "k") (TVar "v"))); IsMutable = false }
        "map-remove", {Scheme = Scheme(["k"; "v"], [], makeFunType [makeMapType (TVar "k") (TVar "v"); TVar "k"] (makeMapType (TVar "k") (TVar "v"))); IsMutable = false }
        "map-contains?", {Scheme = Scheme(["k"; "v"], [], makeFunType [makeMapType (TVar "k") (TVar "v"); TVar "k"] boolType); IsMutable = false }
        "map-has-key?", {Scheme = Scheme(["k"; "v"], [], makeFunType [makeMapType (TVar "k") (TVar "v"); TVar "k"] boolType); IsMutable = false }
        "map-count", {Scheme = Scheme(["k"; "v"], [], makeFunType [makeMapType (TVar "k") (TVar "v")] intType); IsMutable = false }
        "map-empty?", {Scheme = Scheme(["k"; "v"], [], makeFunType [makeMapType (TVar "k") (TVar "v")] boolType); IsMutable = false }
        "map-clear", {Scheme = Scheme(["k"; "v"], [], makeFunType [makeMapType (TVar "k") (TVar "v")] (makeMapType (TVar "k") (TVar "v"))); IsMutable = false }
        "map-keys", {Scheme = Scheme(["k"; "v"], [], makeFunType [makeMapType (TVar "k") (TVar "v")] (makeSeqType (TVar "k"))); IsMutable = false }
        "map-values", {Scheme = Scheme(["k"; "v"], [], makeFunType [makeMapType (TVar "k") (TVar "v")] (makeSeqType (TVar "v"))); IsMutable = false }
        "map-merge", {Scheme = Scheme(["k"; "v"], [], makeFunType [makeMapType (TVar "k") (TVar "v"); makeMapType (TVar "k") (TVar "v")] (makeMapType (TVar "k") (TVar "v"))); IsMutable = false }
        // Every callback below takes the *pair*, as one argument. A Map's
        // element is its `(Tuple %k %v)`: `Iterable`'s `%elem` and `Foldable`'s
        // `%item` say so, and so do `map->list`, `map->seq`,
        // `map-cursor-current` and the `#map(...)` literal. A trait signature
        // mentioning one element takes a one-argument callback, so a
        // two-argument function over a key and a value could not be passed
        // where one is expected.
        "map-merge-with", {Scheme = Scheme(["k"; "v"], [], makeFunType [makeFunType [TTuple [TVar "k"; TVar "v"; TVar "v"]] (TVar "v"); makeMapType (TVar "k") (TVar "v"); makeMapType (TVar "k") (TVar "v")] (makeMapType (TVar "k") (TVar "v"))); IsMutable = false }
        "map-for-each", {Scheme = Scheme(["k"; "v"], [], makeFunType [makeFunType [TTuple [TVar "k"; TVar "v"]] voidType; makeMapType (TVar "k") (TVar "v")] voidType); IsMutable = false }
        "map-iter", {Scheme = Scheme(["k"; "v"], [], makeFunType [makeFunType [TTuple [TVar "k"; TVar "v"]] boolType; makeMapType (TVar "k") (TVar "v")] boolType); IsMutable = false }
        "map-fold", {Scheme = Scheme(["k"; "v"; "s"], [], makeFunType [makeFunType [TVar "s"; TTuple [TVar "k"; TVar "v"]] (TVar "s"); TVar "s"; makeMapType (TVar "k") (TVar "v")] (TVar "s")); IsMutable = false }
        "map-filter", {Scheme = Scheme(["k"; "v"], [], makeFunType [makeFunType [TTuple [TVar "k"; TVar "v"]] boolType; makeMapType (TVar "k") (TVar "v")] (makeMapType (TVar "k") (TVar "v"))); IsMutable = false }
        "map-map", {Scheme = Scheme(["k"; "v"; "v2"], [], makeFunType [makeFunType [TTuple [TVar "k"; TVar "v"]] (TVar "v2"); makeMapType (TVar "k") (TVar "v")] (makeMapType (TVar "k") (TVar "v2"))); IsMutable = false }

        // The one place a pair will not do. `Functor`'s `(-> %a %b)` has to
        // replace the element type and give back the same shape, and the only
        // argument of `(Map %k %v)` free to move is `%v` — so a functorial map
        // over a Map sees the value, with the key riding along.
        "map-map-values", {Scheme = Scheme(["k"; "v"; "v2"], [], makeFunType [makeFunType [TVar "v"] (TVar "v2"); makeMapType (TVar "k") (TVar "v")] (makeMapType (TVar "k") (TVar "v2"))); IsMutable = false }

        // Map conversions
        "list->map", {Scheme = Scheme(["k"; "v"], [], makeFunType [makeListType (TTuple [TVar "k"; TVar "v"])] (makeMapType (TVar "k") (TVar "v"))); IsMutable = false }
        "map->list", {Scheme = Scheme(["k"; "v"], [], makeFunType [makeMapType (TVar "k") (TVar "v")] (makeListType (TTuple [TVar "k"; TVar "v"]))); IsMutable = false }
        "vec->map", {Scheme = Scheme(["k"; "v"], [], makeFunType [makeVecType (TTuple [TVar "k"; TVar "v"])] (makeMapType (TVar "k") (TVar "v"))); IsMutable = false }
        "map->vec", {Scheme = Scheme(["k"; "v"], [], makeFunType [makeMapType (TVar "k") (TVar "v")] (makeVecType (TTuple [TVar "k"; TVar "v"]))); IsMutable = false }
        "seq->map", {Scheme = Scheme(["k"; "v"], [], makeFunType [makeSeqType (TTuple [TVar "k"; TVar "v"])] (makeMapType (TVar "k") (TVar "v"))); IsMutable = false }
        "map->seq", {Scheme = Scheme(["k"; "v"], [], makeFunType [makeMapType (TVar "k") (TVar "v")] (makeSeqType (TTuple [TVar "k"; TVar "v"]))); IsMutable = false }
      ]
      Registry = emptyRegistry
      FunMetas = Map.ofList [
          ("path-combine", { MandatoryCount = 0; KeywordParams = []; RestParam = Some stringType })
          // The recorded element type is the declaration's own rigid variable.
          // That is fine: the call site unifies each rest slot against a *fresh*
          // meta and lets the flat unification against the instantiated function
          // type supply the real one, so `FunMeta` is consulted only for the
          // call's shape. See the comment in `infer`'s structured-call branch.
          ("list", { MandatoryCount = 0; KeywordParams = []; RestParam = Some (TVar "a") })
      ]
      CurrentModule = "" }
