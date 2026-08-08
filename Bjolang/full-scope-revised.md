# Type-directed literal elaboration in Bjolang — revised spec

Supersedes `full-scope-reference.md`. Same goal, same core design; the
differences are recorded in "Changes from the reference spec" at the bottom,
along with the claims in that document that turned out to be false.

Baseline before any change: `./run_tests.sh` → 66 groups, 0 compile failures,
0 execution failures, 5.17s. Every step below must keep that green.

## Context

Bjolang is a Scheme-inspired, statically typed language with Hindley–Milner
inference that compiles to C#. The compiler is written in F#.

| File | Role |
| --- | --- |
| `Lexer.fs` | tokens, including `Quote`, `QuotedSymbol` and `Comma` |
| `Pipeline.fs` | the S-expression reader; `'(…)` → `(quoted-list …)` at line 88, `#map(…)` → `list->map` at line 110 |
| `Parser.fs` | S-expressions → untyped `Expr` / `Decl`; `desugarQuotedList` (427), `processArgs` dispatch (1106) |
| `TypedAST.fs` | `HMType`, `TraitRegistry` (426), `FunMeta` (376), `Env`, `TypedExpr`, `TDecl` |
| `Unification.fs` | `prune`, `unify`, `instantiate`, `generalize` |
| `Inference.fs` | `infer` (903), `EApp` (1255), `EList` (1634), `isSyntacticValue` (764), `registerTypeDefs` (1829), `checkDecl` (1873), `checkModuleValuesAreConcrete` (2771) |
| `Lowering.fs`, `LoopLowering.fs`, `AlphaRename.fs`, `Codegen.fs` | post-inference passes |
| `Prelude.fs` | `emptyRegistry` (23) — the *only* literal `TraitRegistry` construction |

## Goal

Let a literal aggregate elaborate into union constructor applications, driven by
the type expected at the use site. This gives Bjolang list-shaped embedded DSLs
without a macro system.

Given:

```scheme
(type-rec (ProcItem (Union
  (ProcSym  Symbol)
  (ProcStr  String)
  (ProcInt  Int)
  (ProcSub  (List ProcItem))
  (ProcFn   (-> Stream Stream Int)))))

(: run (-> (List ProcItem) Int))
```

this must compile:

```scheme
(run '(pipe (grep "foo" "log.txt") (wc "-l")))
```

by elaborating to exactly what the user writes by hand today:

```scheme
(run (list (ProcSym 'pipe)
           (ProcSub (list (ProcSym 'grep) (ProcStr "foo") (ProcStr "log.txt")))
           (ProcSub (list (ProcSym 'wc) (ProcStr "-l")))))
```

Unquote participates in the same rule:

```scheme
(def (: a (List ProcItem)) '(wc "-l"))
(run '(pipe (grep "foo") ,a))      ; ,a elaborates to (ProcSub a)
```

and an already-correct element passes through untouched:

```scheme
(run '(,(ProcSym 'pipe) ,(ProcStr "-l")))   ; no constructors inserted
```

## Design decisions

1. **Checking mode, not failure recovery.** Do not run inference, catch the
   `failwith` from `unify`, and retry. `unify` binds `MetaVar.Value`
   destructively, so state is already corrupted at the point of failure, and the
   failure site does not identify the repair site. Add a checking-mode function.

2. **Literal aggregate forms only.** A literal aggregate form — `(list …)`,
   `[…]` (`EVec`), a tuple, a record construction, a quoted literal — is
   *checked* against the expected type when one is available, and inferred
   bottom-up when one is not. Ordinary expressions keep today's synthesis-only
   inference. This confines the loss of principal types to syntactic forms
   written directly at the use site.

3. **Unify first, inject second.** At every node, first try the node's natural
   type against the expected type. Only if that fails do you look for a
   constructor. Every program that compiles today still compiles and still means
   the same thing.

4. **An unresolved expected type falls back to synthesis — it is not an error.**
   *This reverses the reference spec's decision 4, which was unimplementable as
   written.* If the expected type prunes to an unbound metavariable, infer the
   literal bottom-up exactly as today and unify. Only if *that* fails is it an
   error, and the message asks for an annotation.

   The reference spec said to refuse outright. That contradicts decision 3:
   `(f '(1 2 3))` at `f : (-> (List %a) Int)` compiles today, with `%a` pinned
   by the literal, and refusing on `TMeta` would break it. Injection still never
   happens against an unbound meta — there is nothing to search — so nothing is
   guessed and nothing is deferred. Synthesis just remains available as the
   fallback it already is.

5. **Depth 1 only.** Insert at most one constructor per node. Do not search
   chains (`String → PathSeg → ProcPath`). Structural recursion through
   list/tuple *shape* is fine; constructor chaining is not.

6. **Ambiguity is an error, and `#:literal` resolves it.** If two cases of the
   same union carry the same payload type, injection for that payload type is
   ambiguous. A case may be marked `#:literal` to be the designated target:

   ```scheme
   (ProcStr String #:literal)
   (ProcPath String)              ; requires explicit ,(ProcPath "...")
   ```

   At most one `#:literal` per union per *top-level type constructor* of the
   payload. "Same payload type" is not a usable key once metavariables are in
   play — see step 1.

7. **`isSyntacticValue` is not modified.** Injection turns `TListMake` of
   literals into `TListMake` of `TApply`, and `isSyntacticValue`
   (`Inference.fs:764`) says an application is never a value. So an elaborated
   literal is not generalized where the un-elaborated one would have been.

   This costs nothing and must not be "fixed". Injection only fires when the
   expected type is known and concrete (decision 4 — an unbound meta falls back
   to synthesis and injects nothing), and a concrete type has nothing to
   generalize. Widening `isSyntacticValue` to admit constructor applications
   would instead push some currently-legal `(def x (SomeCtor …))` into the
   `checkModuleValuesAreConcrete` error, which is a real regression for no gain.

## Non-goals

- No implicit coercion outside the literal aggregate forms.
- No subtyping, no coercion chains, no user-defined conversion trait.
- **No map literals.** Dropped entirely — see step 3c.
- **No constant hoisting in `Codegen`.** The constant-ness *predicate* is
  implemented (step 3); the `static readonly` emission is not.
- No change to `Lowering`, `LoopLowering` or `Codegen` beyond what steps 3b and
  3c require. `AlphaRename` and `LetRecify` do need the new `EQuote` node added
  to their exhaustive matches; the F# compiler will find those for you.

---

## Step 1 — union table in the registry

`registerTypeDefs` (`Inference.fs:1856`) registers each union case as a binding
in `Env.Bindings` and discards the union structure. Add to `TraitRegistry` in
`TypedAST.fs`:

```fsharp
/// Union type name -> (type parameters, cases as (caseName, payload types, isLiteral)).
Unions: Map<string, string list * (string * HMType list * bool) list>
```

Populate it in the `| Union cases ->` branch alongside the existing constructor
binding registration.

**There is exactly one literal `TraitRegistry` construction to update:**
`Prelude.emptyRegistry` at `Prelude.fs:23`. Everything else is a `{ reg with … }`
copy. The reference spec claimed several sites including `Pipeline.fs`; that is
wrong.

Add a helper next to it:

```fsharp
/// Cases of `unionName` whose payload is exactly one field matching `payload`,
/// after substituting the union's type arguments.
member this.CandidateCases : string -> HMType list -> HMType -> (string * HMType list) list
```

**Matching is non-destructive and must never bind a `MetaVar`.** It runs
speculatively. Model it on `ResolveAssociatedType`'s `matchTypes`, with one
addition that `matchTypes` does not have and that is essential here:

> **An unbound metavariable in the queried payload type matches anything.**

Without it the central case fails. A nested `'(…)` at expected `ProcItem` has
natural type `TCon("List",[?m])` with `?m` unbound, and the candidate payload is
`(List ProcItem)`; `matchTypes` has no clause for a meta on the concrete side and
falls through to `| _ when pat = conc -> None`.

The cost is accepted deliberately: a union carrying both
`ProcSub (List ProcItem)` and `ProcNums (List Int)` makes *every* nested literal
ambiguous, because `(List ?m)` matches both. That is reported as an ambiguity
error naming both candidates and stating that the nested literal's element type
could not be determined. `#:literal` is the escape hatch. Resolving it properly
would mean synthesizing the sublist bottom-up and retrying — deferred.

## Step 2 — no work needed; one caveat to respect

Recursive unions already work. `registerTypeDefs` pre-registers every name in a
type group into `LocalTypes` before resolving payloads, and `Codegen` emits a
union as an `abstract record` with `sealed record` subclasses — reference types,
so C#'s "a struct may not contain itself" rule never applies. Function payloads
become `Func<…>`.

**Caveat:** the union and record emission branches in `Codegen.fs` (~2464)
resolve payload types with `Prelude.emptyRegistry`, which has no `Aliases`. A
type alias used *inside a type definition* therefore does not resolve, and the
emitted C# names a type that does not exist. Write payload types out
structurally:

```scheme
(ProcSub (List ProcItem))       ; good
(ProcSub ProcList)              ; broken if ProcList is an alias
```

Pre-existing and out of scope, but do not introduce an alias into `ProcItem`
while testing or you will chase a bogus C# compile error.

## Step 3 — `EQuote` in the parser

Today the reader rewrites `'(…)` to `(quoted-list …)` (`Pipeline.fs:88`) and
`desugarQuotedList` (`Parser.fs:427`) expands it into `Cons`/`Nil` chains during
parsing, so inference never sees the literal. Add:

```fsharp
and Quoted =
    | QInt      of string * Range
    | QString   of string * Range
    | QSymbol   of string * Range      // a bare symbol inside the quote
    | QList     of Quoted list * Range
    | QVec      of Quoted list * Range // [a b] inside a quote
    | QTuple    of Quoted list * Range // dotted pair
    | QUnquote  of Expr * Range        // ,expr
    | QSplice   of Expr * Range        // ,@expr
```

and `EQuote of Quoted * Range` to `Expr`. Update `exprRange` and every
exhaustive match over `Expr`.

**There is exactly one quotation form.** Bjolang has no separate quote and
quasiquote. `'` *is* quasiquote; there is no backtick and no inert-datum form.
Bjolang lists are typed runtime data, not code — there is no `eval` and no macro
expander — so a symbols-only literal has no use that would justify a second
sigil.

Constant-ness is *derived*, not declared: a literal containing no `QUnquote` or
`QSplice` anywhere in its tree is a constant. Implement that as a structural
predicate over `Quoted`. **Do not add the `Codegen` hoisting** — the predicate is
what the design reasoning needs; `static readonly` emission is an optimisation
in a file this change is meant to stay out of.

**Two reading modes.** `'(…)` is the only quoting context. Everything else —
`(list …)`, `[…]`, any ordinary call — is an evaluating context: bare
identifiers are variables and a symbol value requires `'foo`. Nothing is
implicitly quoted outside `'(…)`.

Inside `'(…)` the quoting reader continues through nested brackets, so
`'(cmd [a b])` yields a `QVec` of *symbols*, not of variables. One rule: inside
`'(…)`, everything is quoted until a `,`.

**Commas are context-sensitive. This reverses the reference spec.** The
reference spec required a comma outside `'(…)` to be a parse error. It never
mentioned that `Lexer.Comma` already exists and is deliberately consumed as an
*optional argument separator* in at least six places — `processArgs`
(`Parser.fs:693`), `parseNewDefunArgs` (2260), keyword/positional splitting
(1347), and others. Making it an error would delete working syntax.

So:

- Inside `'(…)`: `,` is unquote, `,@` is splice.
- Everywhere else: `,` remains the skipped separator it is today. Unchanged.

The corpus is in fact clean — stripping comments and strings from every `.bjo`
in `TestFiles` and `lib` finds zero real comma uses — so the stricter rule would
have been *safe*. It is simply not worth the removal.

**Unquote depth rule — implement exactly this.** `,` always evaluates, at every
depth, and the comma count is always one. Nesting a quote does not raise a level
and does not make an inner `,` inert:

```scheme
'(a '(b ,x))     ; the inner ' is a nested literal list.
                 ; ,x evaluates. Always. No level counting.
```

Scheme's quote-level arithmetic exists to serve macros that write macros;
Bjolang has neither.

`,,x` (two or more commas) must be a **parse error** with the message
`Nested unquote is not supported; ',' always evaluates.` Not peel-one-comma, not
Scheme's semantics, not silent acceptance. Reserving it keeps a future staged
template feature free to choose. A user who wants a retained hole writes it as
ordinary data — `,(ProcHole 'pattern)` — and walks the tree themselves.

**The `EIdent` → `QSymbol` meaning change is real but nearly unobservable.**
`desugarQuotedList` currently turns a bare `Symbol` into `EIdent`, so `'(pipe x)`
today means the *variable* `pipe`; under `QSymbol` it means the `Symbol` value.
Audit anyway, but the surface is tiny: all 38 `'(` occurrences in the corpus are
lists of integer literals plus one `'()`, and not one contains a bare symbol.
`16_quote_and_tuple.bjo` even carries a comment saying nothing else uses `'(…)`.
`'foo` on a bare symbol is a different lexer path (`QuotedSymbol`) and is
unaffected — `(eq? x 'foo)` must keep working exactly as it does today.

**Document `'(…)` as sugar, not as a special form.** With `(list …)` also
checked, these are the same program and must elaborate identically:

```scheme
(run '(pipe (ls "-l")))
(run (list 'pipe (list 'ls "-l")))
```

## Step 3b — `list`, which does not exist yet, as a real function

`EList` is in the `Expr` union (`Parser.fs:120`), has an inference case
(`Inference.fs:1634`) and emits correctly (`TListMake`, `Codegen.fs:974`,
`TypeVisitor.fs:67`) — but **nothing in the parser ever constructs it**. There is
no `"list"` case in the `processArgs` dispatch alongside `"Tuple"`,
`"quoted-list"` and `"vec-literal"`, and no prelude binding named `list`. The
node is orphaned. Wire it up before step 4.

**`list` is a real prelude function, not a special form.** *This reverses the
reference spec's step 3b, which chose special-form-only and then left tests 12
and 13 in the list, both of which require the opposite.*

Add to `Prelude.fs`:

```scheme
(: list (-> #:rest %a (List %a)))
```

with a matching `FunMeta` (`MandatoryCount = 0`, `RestParam = Some (TVar 'a)`).
Variadic call sites then already work through the existing structured-call
branch at `Inference.fs:1287` — no syntactic special case is needed for calling.

**Checking is driven by a marker on the binding, not by the name.** Mark the
prelude `list` binding as a builder. `EApp` then unifies the callee's *return*
type with the expected type first, and checks each argument against its
now-pinned parameter type instead of inferring it. That is ordinary
bidirectional application typing.

This is the reference spec's step 5b mechanism, pulled forward — but **internal
only**. Implement the marker and the typing rule; do *not* add `#:builder`
surface syntax, and do not implement its declaration-time rejection rules (trait
methods, return types that do not mention the parameter variables). Those matter
only once users can write the marker themselves. Deferred, not cancelled.

The marker also **replaces the reference spec's shadowing guard**, and is
strictly better than it: `(let ([list f]) (list 'a "b"))` rebinds the name to a
binding that carries no marker, so elaboration simply does not fire. There is no
"does this still resolve to the prelude binding?" test to write, and therefore
none to get wrong.

**The rest-arg path is where the real change lands.** `Inference.fs:1318-1325`
currently does:

```fsharp
let elemSlot = freshMeta ()
for (rt, _) in restArgs do
    unify env.Registry rt elemSlot
```

That single `elemSlot` is exactly the "the signature forces one element type by
construction" problem. For a builder callee, pin `elemSlot` from the expected
type *first*, then `check` each rest argument against it.

**Codegen: a real implementation, plus a peephole.** `list` must exist as an
emitted function for value-position uses. Add a lowering peephole so a saturated
direct call to the marked prelude `list` becomes `TListMake` — keeping the fast
nested-`Cons` codegen for the common case.

This peephole reintroduces a "is this the prelude `list`?" check, but at a point
where it is a **pure optimisation**: a wrong answer costs an array allocation,
never a mistyped program. That is a far safer home for such a test than the type
checker.

**`(list …)` and `'(…)` must lower identically.** `desugarQuotedList` builds
`Cons`/`Nil` chains; `EList` builds `TListMake`. At the C# level these already
agree — `TListMake` emits nested `SchemeList.SchemeList.Cons(…)` terminated by
`Empty<T>()` (`Codegen.fs:974-987`), and the prelude `Cons`/`Nil` are the same
runtime thing. (The reference spec's warning about "different runtime
representations" was unfounded; existing `'(1 2 3)` tests cannot silently change
behaviour.) What differs is the *typed tree*, so route quoted literals through
`TListMake` for the identical-trees assertion.

### Step 3b-2 — `FunMeta` for `def`

`DDef` never registers a `FunMeta`, not even when a signature exists. Only
`DDefun` (`Inference.fs:1993`), `DExtern` (2246) and imported module metadata
(2728) do. So an aliased variadic function cannot be called variadically.

In `checkDecl`'s `DDef` case, when the signature is a `TArrow` carrying rest or
keyword params, register a `FunMeta` — mirroring `DExtern` at
`Inference.fs:2239-2248` exactly. This is the same rule `extern` and `import`
already follow, extended to `def`.

**Two obstacles stand between `(def f list)` and `(f 1 2 3 4 5)`; this fixes
only the first.** The second is `checkModuleValuesAreConcrete`
(`Inference.fs:2771`), which rejects any module-level `TDef` whose type has free
variables, with no exemption for function types — only `defun` is exempt,
because a generic *method* is ordinary C# while a generic static *field* has
nowhere to declare its parameter. So the alias needs a monomorphic signature:

```scheme
(: f (-> #:rest Int (List Int)))
(def f list)
(f 1 2 3 4 5)          ; ✓
```

That restriction is pre-existing, correct, and out of scope. An un-annotated
`(def f list)` at module level is an error today and stays one.

## Step 3c — vector literals only

`[…]` already parses to `EVec` (`Parser.fs:1112`). It needs no new syntax, only
the `check` case in step 4.

**Map literals are out of scope.** The reference spec said they "do not exist
yet" and asked for lexer work to pin the syntax. Both are wrong: `#map(…)` and
`#map[…]` already lex (as a single `Symbol "#map"`, not `Hash` + `map`) and read
(`Pipeline.fs:110-113`). The real obstacle is the opposite of the one described —
`desugarMapLiteral` (`Pipeline.fs:24`) rewrites them at *read* time into
`(list->map (Cons (Tuple k v) …))`, so there is no `EMap` node for a checker to
see, and creating one means adding a node to `exprRange`, `AlphaRename`,
`LetRecify`, `Inference`, `Lowering` and `Codegen`, all of which it currently
gets for free.

Dropped on the spec's own reasoning: a map is homogeneous in its value type, so
injection helps only when that type is a union — narrow. A literal wanting a
different type per key is a record, which step 4 covers.

Duplicate-key detection and last-wins indexer semantics remain worth doing to
`#map` — separately, and unrelated to this feature.

## Step 4 — `check`

Add to `Inference.fs`, mutually recursive with `infer`:

```fsharp
and check (env: Env) (expected: HMType) (expr: Expr) : TypedExpr =
    match expr with
    | EQuote(qq, r) -> checkQuoted env expected qq r
    | _ ->
        let t, te = infer env expr
        unify env.Registry t expected
        te
```

`checkQuoted env expected node`, where `expected` is `prune`d first:

- `QList(items)` against `TCon("List",[elem])` → `TListMake` of
  `check env elem` on each item. `QSplice` items contribute their elements.
- `QVec(items)` against `TCon("Vec",[elem])` → same, as `TVecMake`.
- `QTuple(items)` against `TTuple ts` → elementwise.
- Any node against expected `T`:
  1. Compute the natural type: `QInt` → via `inferNumericType`, `QString` →
     `stringType`, `QSymbol` → `symbolType`, `QList` → `TCon("List",[fresh])`,
     `QUnquote e` → `infer env e`.
  2. If the natural type matches `T` non-destructively, commit that: emit the
     node directly (a `QUnquote` emits its own typed expr unchanged).
  3. Else, if `T` is `TCon(u, args)` with `u` in `Registry.Unions`, call
     `CandidateCases u args natural`. Exactly one → emit
     `TApply(TIdent(caseName, …), [elaborated payload], [])`, recursing into the
     payload with the case's declared payload type as the new expected type.
     Zero → error. Two or more, none `#:literal` → ambiguity error.
  4. Else → error.
- Expected type prunes to `TMeta` → **synthesize bottom-up and unify** (decision
  4). Build a homogeneous list, unify all elements into one `elem` meta, exactly
  as today. A heterogeneous literal then fails with the annotation message.

**Then generalize `check` to the other literal aggregates.** `checkQuoted` and
these are the same function over different node types — factor out the shared
"natural type → match, else inject" core and call it from each:

| node | expected type | behaviour |
| --- | --- | --- |
| `EList` (`(list …)`) | `TCon("List",[elem])` | check each element against `elem` |
| `EVec` (`[…]`) | `TCon("Vec",[elem])` | same |
| `ETuple` | `TTuple ts` | elementwise, each slot its own expected type |
| record construction (`Inference.fs` ~1074) | record type | check each field against `expectedFieldsInstantiated` |

Records are the highest-value case after lists.

## Step 5 — thread the expected type at call sites

Three call sites need it. **`EApp` is the important one** — it is what makes
`(run '(...))` work with no annotation.

**`EApp` general case (`Inference.fs:1255`).** Today `splitArgs` infers every
argument eagerly, before `targetType` is unified with the argument types. Split
into two passes:

1. Infer all arguments that are **not literal aggregate forms**. Leave holes for
   the ones that are, each holding a fresh meta.
2. Unify `targetType` with `TFun(argTypes, retType)` as today. This pins as much
   of each parameter type as the other arguments and the callee's signature can.
3. `prune` each hole's parameter type and `check` the corresponding argument
   against it.

**The deferral predicate is "is a literal aggregate form", not "is an
`EQuote`".** *The reference spec said `EQuote` here and then made `EList`,
`EVec`, `ETuple` and record construction checked forms in step 4 without coming
back to fix this.* As written, `(run (list 'wc "-l"))` could not work, because
`list` would be inferred eagerly against a fresh meta.

Ordering matters for generic callees: with `(: run2 (-> (List %a) %a Int))`,
step 2 lets the second argument pin `%a` before the literal is elaborated.
Preserve the existing keyword/rest handling in the structured-call branch;
literal-aggregate arguments in keyword and rest positions go through the same
deferral.

**`DDef` in `checkDecl` (`Inference.fs:1877`).** Today: `infer`, then unify
against `sigs`. Look up `sigs` *first* and `check env sigType expr` when a
signature exists. This is what makes `(def (: a (List ProcItem)) '(...))`
elaborate. `(: bleh (List int))` + `(def bleh '())` must still work — it is the
fix named in `TestFiles/errors/module_value_open_type.bjo`.

**`DDefun` return position and annotated `let`.** Same treatment: where a
declared type is already in hand, `check` instead of `infer`-then-`unify`.

## Step 6 — errors

Every error must carry the **original** `Range` of the offending literal node,
not the elaborated node's. Required messages:

| Situation | Message |
| --- | --- |
| no candidate | `No case of ProcItem carries String (line N). Expected ProcItem here.` |
| ambiguous | `String matches both ProcStr and ProcPath in ProcItem (line N). Mark one #:literal, or write the constructor explicitly.` |
| ambiguous, undetermined element type | `A nested list literal here could be ProcSub or ProcNums in ProcItem (line N); its element type could not be determined. Write the constructor explicitly.` |
| synthesis fallback failed | `(list ...) was inferred without an expected type at line N, so its elements must share one type. Annotate the binding as (List ProcItem) to construct a union.` |
| expected type not a union | `Expected Int but this is a String (line N).` |

Never emit union-flavoured wording when the expected element type is not a
union — `(list 1 "a")` at `(List Int)` must say *expected Int, got String*, not
*no case of Int carries String*. Keep those error paths separate.

## Tests

1. The `run` example above, end to end through `Codegen`, executed.
2. Explicit `,(ProcSym 'pipe)` still compiles unchanged (decision 3).
3. `(def (: a (List ProcItem)) '(wc "-l"))` then `(run '(pipe ,a))` → `,a`
   wrapped in `ProcSub`.
4. Nested lists three deep.
5. `,@` splicing a `(List ProcItem)` into a quoted literal.
6. A quoted literal in a `let` with no annotation → synthesis fallback; a
   *homogeneous* one succeeds, a heterogeneous one gives the annotation error.
   Not a crash and not a wrong guess.
7. Two cases carrying `String` without `#:literal` → ambiguity error; with
   `#:literal` on one → compiles.
8. A generic callee `(: run2 (-> (List %a) %a Int))` where the second argument
   pins `%a` — verifies the step 5 argument ordering.
9. A quoted literal against a non-union expected type, e.g. `(List String)`,
   still works by plain unification with no constructors inserted.
10. `(run (list 'wc "-l"))` — the `(list …)` form, no quoted literal.
11. `(list 'pipe (list 'ls "-l"))` at `(List ProcItem)` — nested `(list …)`
    injecting `ProcSub`.
12. `(let ([list f]) (list 'a "b"))` — shadowed `list` does *not* elaborate.
13. **`list` as a first-class value.** *Replaces the reference spec's
    `(fold list nil xs)`, which cannot typecheck: `list` is unary — `#:rest`
    resolves to `TFun([Array a], List a)` — and `fold` needs a binary function
    with two differently-typed arguments.*

    ```scheme
    (: f (-> #:rest int (list int)))
    (def f list)
    (expect "aliased variadic" (same? (f 1 2 3 4 5) (list 1 2 3 4 5)))
    ```

    Pins four things: `list` has a real binding rather than being a special
    form; the binding can be aliased; the alias spreads variadically at its own
    call sites; both spellings agree on the result.
14. `list` in value position at the array type — `(def g list)` under
    `(: g (-> (Array int) (list int)))`, then `(g (array 1 2 3))`.
15. An un-annotated module-level `(def f list)` still produces the existing
    open-type error. Documents step 3b-2's second obstacle as intended.
16. A record construction whose fields are elaborated per-field.
17. A tuple at `(Tuple Symbol ProcItem)` — each slot checked separately.
18. `(list 1 "a")` at `(List Int)` — error says *expected Int, got String*, with
    no mention of unions.
19. `'(a '(b ,x))` — the inner `,x` evaluates; no level counting.
20. `,,x` — parse error, not peel-one and not Scheme semantics.
21. `'(pipe (ls "-l"))` and `(list 'pipe (list 'ls "-l"))` elaborate to
    identical typed trees modulo ranges.
22. A literal with no unquotes satisfies the constant predicate; the same
    literal with one `,` does not. Predicate only — no hoisting is asserted.
23. `[…]` at `(Vec ProcItem)` — vector literal elaborates like a list.
24. `'(cmd [a b])` → `QVec` of symbols, not of variables.
25. A comma *outside* `'(…)` is still an argument separator, unchanged.
26. Regression: `'foo` on a bare symbol unchanged, and every existing `'(...)`
    in the corpus still means what it meant.

Dropped from the reference list: map literals (22, 23), `#:builder` surface
syntax and its rejection rules (24–28), and the coercion-hazard tests that only
make sense once `#:builder` is user-writable (25, 26).

## Invariants to preserve

- No `TplType` reaches `unify`, `generalize` or `Codegen`.
- Trait obligations are still discharged (`solvePending`) before generalization.
- `isSyntacticValue` is unmodified, and the value restriction in `DDef` and
  `let` is unchanged.
- `CandidateCases` never mutates a `MetaVar`.
- Elaboration is a pure function of the source node and the pruned expected type
  — running it twice on the same inputs gives the same output.

---

## Implementation progress

**Done: step 1, step 3b, step 3b-2.** Suite at 67 groups, 0 failures (was 66;
`66_list_builtin.bjo` is new).

- **Step 1** — `Unions` on `TraitRegistry`, populated in `registerTypeDefs`,
  plus `CandidateCases`. Verified by probe before removal: `MyOption<int>` given
  `int` yields `MySome`, given `string` yields nothing, and given `(List ?m)`
  yields nothing; `MyOption<(List int)>` given `(List ?m)` yields
  `MySome` carrying `(List int)` — the nested-literal case the wildcard exists
  for — with the metavariable still `Value = None` afterwards, confirming the
  match is non-destructive.
- **Step 3b** — `list` is a prelude binding with a `FunMeta`, backed by
  `BjolangRuntime.list<T>(params T[])`.
- **Step 3b-2** — `DDef` registers a `FunMeta` from a `TArrow` signature.

### Deferred out of these steps

- **The `TListMake` peephole.** Its only purpose is making `(list …)` and
  `'(…)` produce identical typed trees, and `'(…)` does not produce anything
  yet. Doing it now also meant writing a "is this the prelude `list`?" guard
  that step 4's builder marker is supposed to replace. It belongs with step 4.
  Until then `(list 1 2 3)` emits a real call, which is correct but not the
  nested-`Cons` chain.
- **`#:literal` parsing.** `Unions` carries the flag and `CandidateCases`
  honours it, but nothing sets it — `parseType` has no `Keyword` case, so
  `(ProcStr String #:literal)` is still a parse error. It is only consulted on
  ambiguity, which arrives with step 4. Note that `DataCase` has no field for
  it, so this also needs a decision about how the marker survives module export
  (`Program.fs:422`).
- **The builder marker itself.** Only `check` consumes it. Step 4.

### Three bugs found while implementing, all pre-existing

None of these were in the reference spec; all were exposed rather than caused by
this work, and all are fixed.

1. **A rest call's typed tree contradicted its own type.** `TApply` was handed
   the rest arguments spread flat — N of them — against a function type with one
   `(Array %a)` parameter, and relied on C# `params` to reassemble them at the
   call site. That works only when the callee is emitted as a real `params`
   method. Alias the function to a value and the callee is a `Func<int[], …>`
   field; delegates have no `params` semantics, so `(f 1 2 3)` passed the type
   checker and then produced C# that would not compile. The structured-call
   branch now materializes a `TArrayMake`, as `LoopLowering` already did for
   tail calls into a rest parameter. C# accepts an explicit array for a `params`
   parameter, so the direct case is unchanged.

2. **Shadowing did not clear `FunMeta`.** `FunMetas` is keyed by name and
   `addBinding` only wrote `Bindings`, so a local binding or parameter named
   after a variadic function left the old shape behind. `(let ((list …)) (list 5))`
   failed with `Cannot unify Int32 with Array<Int32>` — an error naming a type
   the program never mentions. `addBinding` now removes the entry; callers that
   introduce a genuinely variadic binding re-add it immediately, as `DDefun` and
   `DDef` both do. This was reachable before via `path-combine`, but adding
   `list` to the prelude made a very ordinary local name trigger it.

3. **Shadowing a prelude function emits C# that does not compile.** A Bjolang
   `let` becomes a C# local, and a C# local's declaration space is the whole
   enclosing method — so a local named `list` sitting beside calls to the
   prelude `list` in the same body gives "cannot use local variable before it is
   declared". The Bjolang program is well typed and unambiguous; the code
   generator is what cannot express it. **Not fixed** — the fix is for
   `AlphaRename` to rename a local that shadows a prelude name it does not
   otherwise know about. `66_list_builtin.bjo` works around it by putting the
   shadow test in its own function, with a comment saying why.

### Error tests now run — and seven of them were dead

`run_tests.sh` only ran `TestFiles/[0-9][0-9]_*.bjo`, so the ten programs under
`TestFiles/errors/` had never been executed. It now runs them as a second phase:
each must be rejected, and a file may pin *why* with one or more

```
;; EXPECT-ERROR: <substring>
```

lines, every one of which must appear in the compiler's output. A file without
one only has to fail.

That distinction earned its keep immediately. **Seven of the ten were
bit-rotted**: they opened with `(import (std core))`, and there is no
`lib/std/core.bjo`, so they died in module resolution without ever reaching the
code they were written to test. A must-fail-only check would have called all
seven green forever. Fixed by importing `(std prelude)`, after which each one
produces a real language error, verified individually.

Expectations are pinned for seven files. Three are must-fail-only because their
intent could not be confirmed against a current message, and they should be
looked at:

- `local_two_constructors.bjo`, `local_two_element_types.bjo` — both now fail
  with a raw `Cannot unify TFun (...)` dump. The impl body calls
  `(list-map xs g)`, but the prelude's `list-map` takes the function first, so
  the test may be failing on its own argument order rather than on the local
  generalization rule it documents.
- `monad_transformer.bjo` — fails with `Invalid type definition at line 10`, a
  parse error. Its comment describes a kind-level restriction, so the
  `(type ((StateT %s %m %a) ...))` syntax may have drifted too.

Both failure modes are covered by the runner and were verified by deliberately
breaking a file: a wrong expectation reports "rejected, but not for the stated
reason", and a program that compiles reports "compiled successfully, but was
expected to be rejected". The suite exits non-zero for either.

## Changes from the reference spec

| # | Reference spec | Revised | Why |
| --- | --- | --- | --- |
| 1 | Unbound expected type → error | → synthesize and unify, error only on failure | Refusing broke decision 3; `(f '(1 2 3))` at `(-> (List %a) Int)` compiles today |
| 2 | `list` is a special form, not first-class | `list` is a prelude function with a builder marker, plus a lowering peephole | Requested; the reference spec chose special-form-only yet kept tests requiring the opposite |
| 3 | Comma outside `'(…)` is a parse error | Comma stays an argument separator outside `'(…)` | `Lexer.Comma` is already a separator in ~6 parser sites; the error deletes working syntax for a weak diagnostic |
| 4 | Shadowing guard on the resolved binding | Marker on the binding | Markers travel with bindings through shadowing; no guard to get wrong |
| 5 | `CandidateCases` modelled on `matchTypes` | …plus "unbound meta matches anything" | `matchTypes` has no meta clause and fails the central nested-list case |
| 6 | Deferral predicate is `EQuote` | …is any literal aggregate form | Step 4 makes `EList`/`EVec`/`ETuple`/records checked; test 10 is impossible otherwise |
| 7 | Map literals: pin syntax, extend lexer | Dropped | They already exist; the real work is *removing* a read-time desugaring |
| 8 | `Codegen` hoists constants to `static readonly` | Predicate only | Contradicted the spec's own non-goal of not touching `Codegen` |
| 9 | `#:builder` as user-facing syntax with rejection rules | Mechanism internal, syntax deferred | The mechanism is needed for `list`; the syntax and its rules are not |
| 10 | Test 13 `(fold list nil xs)` | Aliased-variadic test | `(fold list nil xs)` cannot typecheck — `list` is unary |
| — | — | New: `DDef` registers `FunMeta` from a `TArrow` signature | Without it an aliased variadic cannot be called variadically |

### Claims in the reference spec that are false

- **"Map literals do not exist yet"** and "`#map(` would lex as `Hash` followed
  by the symbol `map`". Both wrong. `#map(…)` lexes as one `Symbol "#map"` and
  reads via `desugarMapLiteral` (`Pipeline.fs:24, 110-113`).
- **"Update every record construction of `TraitRegistry` (there are several;
  `Prelude.fs` and `Pipeline.fs` too)."** There is exactly one:
  `Prelude.emptyRegistry` (`Prelude.fs:23`).
- **"These are different runtime representations"** (of `TListMake` versus the
  `Cons`/`Nil` chain). They are the same at the C# level; only the typed tree
  differs.
- **"This changes the meaning of existing programs"** (the `EIdent` → `QSymbol`
  change). True in principle, empty in practice: no `'(…)` in the corpus
  contains a bare symbol.
- **`(fold list nil xs)`** as a motivating first-class use. Ill-typed regardless
  of this feature.

### Known gaps carried forward deliberately

- Variadic functions are not first-class in Bjolang for *any* function: `#:rest`
  resolves to a unary `Array` parameter and spreading is a `FunMeta` lookup
  keyed by name, firing only when the callee is a bare `EIdent`
  (`Inference.fs:1261`). An un-annotated `(def f list)` therefore cannot spread.
  Fixing it means either propagating `FunMeta` from a bare-identifier RHS *and*
  extending registration to `let` bindings, or carrying rest shape in `TFun`.
  Neither is in scope.
- Nested-literal ambiguity is over-reported when a union has two cases with the
  same outer type constructor (step 1).
- Rest-args hide arity mistakes: with `ProcSub of (List ProcItem)`,
  `(list (list 1 2 3))` is one wrapped element and `(list 1 2 3)` is three. Both
  typecheck. Before injection, one would have been an error. Inherent.
