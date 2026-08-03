# Implementation Task: A Trait-Based Loop Facility (Bjolang)

You are an expert F# compiler engineer working on **Bjolang**, a Lisp-like language with HM inference and traits that compiles to C#.

Build a `loop` facility in the style of goof-loop / foof-loop where **sequences and accumulators are extended by writing trait impls rather than macros**.

**The mental model:** `loop` is a glorified left fold with early exit that *always* delivers a result. Every exit path — exhaustion, `:break`, or a named loop declining to iterate — routes through the same finish block. This is why accumulators are hoisted: they must hold state across the whole loop and be visible in the finish block.

**Out of scope:** specialized variants (`loop/list`, `loop/vector`, …) and a `loop/do` form restoring a body position are being designed separately. Spec and build the general form only.

**Depends on:** `ImplTarget` patterns, the `ResolveAssociatedType` fix, and default method bodies in traits. All blocking — see Prerequisites.

> Clause semantics were reconstructed and were a proposal. The four open points
> are now settled — see "Settled semantics" below. The protocol and the
> compilation strategy are the load-bearing parts.

---

## Settled semantics

All four fall out of the single left-to-right level-assignment pass, because the
level counter increments only when a `:for` is *reached*.

1. **An `:acc` before a nested `:for` belongs to the outer level.** A `:for`
   preceded by anything other than a `:for` opens a new level; every clause
   above it — `:acc` included — belongs to the level that was current at its own
   position. So its step runs once per *outer* iteration.
2. **Lockstep `done?` runs in clause order and short-circuits.** The `:for`
   clauses of a level are tested in the order written; the first one that is
   done ends the level, and no later clause's `done?` is called. This is what
   makes the "exactly once per iteration" contract implementable with effectful
   cursors.
3. **A `:for` pattern is destructuring only and must always match.** It is not a
   filter and cannot fail. A pattern that could fail — one constructor of a
   multi-constructor union — is a compile error naming the irrefutable subset,
   rather than a match that throws at runtime.
4. **`:subloop` affects only subsequent `:for` clauses.** It behaves as
   `(:when #t)` in that it breaks `:for` adjacency, but it emits nothing. A
   `:let` written between a `:subloop` and the `:for` it opens stays in the
   *outer* level, which is both what the original implementation did and what
   the level-assignment pass gives for free.

## Implementation status

**Stage 1 is done and covered by `TestFiles/39_loop.bjo`.** One level:
`:for` (several = lockstep), `:let`, `:do`, `:when`, `:acc` with `#:when`, `=>`
and the auto-generated finish. Sequences are `List` and `Vec`; collectors are
`listing`, `summing`, `counting` and `folding`, all in `(std iter)`.

- The prologue binds sequences and collectors with `let/mono` (prerequisite 6,
  built as `ELetMono`).
- `(loop ...)` is recognized only when its first argument is a keyword-headed
  list, so `(loop (+ i 1))` still means a call to a named `let` called `loop`.
- `LoopLowering.assertLoopsPromoted` runs after loop lowering and fails the
  build if a generated loop still refers to itself by name — that is, if its
  recursive edge was not in tail position. Reaching `TLoop` is *not* the
  property worth asserting: `lowerLetRec` emits one whenever the members are
  function-shaped, jumps or no jumps.
- Emitted code for `(loop (:for x xs) (:acc out (listing x)) => out)` is a plain
  `while (true)` with the cursor and collector protocols inlined away entirely —
  `listsubempty_QMARK` / `listsubhead` / `listsubtail`, `Cons`, `listsubreverse`.
  One allocation survives per loop *entry*: the collector value itself, which is
  dead after inlining and would need a DCE pass to remove.

**`:break`, `:final` and named loops are done too**, covered by
`TestFiles/40_loop_control.bjo`.

- `:break` emits the finish block inline at the clause position. That duplicates
  it once per `:break`, which is what `M_exit`-as-a-group-member is for; with a
  single level there is nothing to jump to yet, and `LetRecify`'s SCC pass would
  split a finish member out of the group anyway, since the loop calls it but it
  does not call back.
- `:final` is exactly the documented rewrite, and it falls out of the clause walk
  rather than needing a pre-pass: test the hidden slot, and on the false branch
  step it and carry on. Its accumulator is a gensym and is marked hidden, so it
  appears in neither the finish block's bindings nor an auto-generated result —
  an author's accumulator called `tmp` is untouched.
- A named loop's final `:do` has its *tail positions* rewritten: one that is a
  call to the loop name becomes the jump, and one that is not runs for its effect
  and falls into the finish block. That is what keeps `(lp)` a genuine tail call
  while still letting a path decline to iterate.
- `(lp #:name expr)` overrides accumulator slots only, and the argument vector is
  completed at desugar time. A `:for` variable is *not* addressable: it is
  derived from its cursor rather than carried, so there is no slot to override.
  Making one addressable would mean carrying an `Option` element slot per `:for`
  and consulting it ahead of `current` — implementable, but it allocates per
  iteration and the semantics against `done?` need deciding, so the compiler
  refuses with a message naming the accumulators it *can* override.

**Nested levels are done**, covered by `TestFiles/41_loop_levels.bjo`, and with
them `:subloop` and `:end-subloop-if`. The flat group of 5c is what is emitted:
one `ELetRec` with a member per level, all mutually recursive — M_i enters
M_{i+1} and M_{i+1} hands back to M_i — so `LetRecify` sees a single SCC and
codegen emits one `while (true) switch` with `goto case` between levels and no
calls at all. A three-level loop is three cases.

Two things the spec did not settle, found by building it:

- **Slot vectors have to carry the enclosing levels.** M_i's slots are every
  level 0..i's cursors, *plus every enclosing level's bindings*, plus every
  accumulator. The cursors because an inner level jumps back to its parent with
  the parent's cursors advanced, and it cannot advance what it does not hold; the
  bindings because a member is a separate function with no lexical view of its
  caller, and an inner sequence or clause usually names an outer loop variable.
  Level 0's sequences are the exception: they are loop-invariant, so they stay in
  the prologue and are lexically in scope for the whole group.

- **`ELetRec` had to unify a member's shape before checking its body.** An inner
  level's sequence arrives as a *parameter*, so its type was a bare metavariable
  while the body was inferred — and an associated-type projection needs a
  concrete head, so `current` on it failed with `Cannot unify int with
  TAssoc ("Iterable", "elem", TMeta ...)`. Inference now unifies
  `TFun(argTypes, _)` with the member's expected type *before* inferring the
  body, so the argument types an earlier member's call site already pinned reach
  the later member. Members are emitted outermost-first, which is the order that
  makes this fire.

`M_exit` is still not a member: `:break` and level 0's exhaustion emit the finish
block inline, so it is duplicated once per exit site. Making it a member is the
remaining piece of 5c, and is now cheap — the group already has several members.

Not implemented: overriding a `:for` variable from a named loop, which needs the
`Option` element slot described above.

## Prerequisite status (audited against the tree)

| # | Prerequisite | Status |
|---|---|---|
| 1 | Default method bodies in traits | **MISSING** — blocks only the Part 2 generic path |
| 2 | `ResolveAssociatedType` substitution | **DONE** — `TypedAST.fs:345-380` matches and substitutes |
| 3 | `Implementations` keeps target args | **DONE** — `ImplTarget { Ctor; FixedPrefix; HoleArity }` |
| 4 | Keyword argument naming | **DONE** — call site and declaration agree on `__kw_` |
| 5 | Multi-member loop groups form | **DONE** — verified: one `while(true) switch`, `goto case`, no calls |
| 6 | Non-generalizing binding | **MISSING** — and required; see below |
| 7 | `record struct` | **MISSING** — performance only |

Prerequisites 2 and 3 were described as blocking and are not: the claims are
stale. 5 was verified rather than assumed — three mutually recursive local
functions each tail-calling the next emit one `while (true) switch` with
`goto case` between members and no calls, and ran 3,000,000 iterations without
touching the stack.

**Prerequisite 6 is real, and neither workaround in 5b survives contact.**
`TestFiles/probe/loop_spike.bjo` is the hand-written target shape. With the
collector bound by an ordinary `let`, the binding generalizes and codegen emits
an unbound type parameter: `Listing<T_t__1> c0 = new Listing<T_t__1>.MkListing();`.
The two escapes both fail:

- Writing the collector as an *application* — `(MkListing)` — to trip the value
  restriction is a type error: a nullary constructor is not a function.
- Binding it as an immediately-applied lambda parameter avoids generalization
  but leaves the parameter a bare metavariable while the body is checked, so the
  associated types cannot project: `Cannot unify TAssoc ("Collector", "elem", TMeta ...)`.

The prologue binding must therefore be **monomorphic *and* concrete-headed at
the point the body is inferred** — which is exactly a non-generalizing `let`:
infer the right-hand side, then bind with `Scheme([], [], t)` instead of
generalizing. Nothing else in the language does both. With that stood in for by
an explicit annotation, the whole spike compiles, runs, and emits a plain
`while` with the entire cursor and collector protocol inlined away.

---

## Part 1 — The `Iterable` trait

```
(trait Iterable (s) (Elem) (Cursor)
  (start   : (s -> Cursor))
  (done?   : (s -> Cursor -> Bool))
  (current : (s -> Cursor -> Elem))
  (next    : (s -> Cursor -> Cursor)))
```

A first-order trait with two associated types. It does **not** need constructor-variable signatures — a sequence is never returned at a new element type — so it exercises the inlining path without depending on the HKT machinery.

Every method takes both the sequence and the cursor. Passing `s` everywhere, rather than folding it into `Cursor`, keeps `Cursor` minimal: a vector's cursor is a bare `Int`, not a `(vector, index)` pair. `s` is loop-invariant, bound once in the prologue, and substituted away by the inliner, so the arguments that go unread cost nothing.

| Sequence | `Cursor` | `start s` | `done? s c` | `current s c` | `next s c` |
|---|---|---|---|---|---|
| `List<a>` | `List<a>` | `s` | `(null? c)` | `(car c)` | `(cdr c)` |
| `Vec<a>` | `Int` | `0` | `(>= c (vec-length s))` | `(vec-ref s c)` | `(+ c 1)` |
| `Range` | `Int` | `(range-lo s)` | `(>= c (range-hi s))` | `c` | `(+ c (range-step s))` |
| `Generator<a>` | *(see Part 2)* | | | | |

There are no `in-list` / `in-vector` wrapper forms. `(:for x xs)` resolves `Iterable` on the type of `xs`. Iteration variants needing extra control are **values with their own impls** — `(:for i (range 0 10 2))`, `(:for x (reversed v))` — not clause syntax.

### Protocol contract

1. `done?` is called **exactly once per iteration**. Nothing peeks, nothing tests at both top and bottom, nothing hoists a redundant check. **`done?` may be effectful** — this is what lets an enumerator-backed cursor fuse test-and-advance, making `next` the identity.
2. `current` is called **only** when the preceding `done?` returned false.
3. The cursor is used **linearly**: never copied, never stepped twice, never captured. This is what lets `next` mutate and return the same object instead of allocating per iteration.
4. `start` is called **once per entry** to the level owning the cursor.

---

## Part 2 — The generic path, without the compiler knowing the trait

When the sequence type prunes to a `TCon`, resolve the impl and inline the cursor protocol. When it prunes to a `TVar`, nothing can be inlined, so the loop consumes the sequence through a single interface method.

That method is a **trait method with a default body**, not something codegen synthesizes:

```
(trait Iterable (s) (Elem) (Cursor)
  (start   : ...)  (done? : ...)  (current : ...)  (next : ...)
  (as-generator : (s -> (Generator Elem))
    <default body: walk the cursor protocol, yielding each element>))
```

Nothing in the compiler mentions `Iterable`, and nothing in the runtime knows about cursors. The emitted C# interface carries `as-generator` alone; `Cursor` never appears in it, which is fine, because it is exactly what an interface cannot express. The generic case works with the existing `_dict_*` / `TInterfaceCall` machinery unmodified — `Iterable` is an ordinary interface trait, not a new kind.

**Why the split is safe.** A mutable struct cursor never crosses a call boundary: on the concrete path it is inlined into the loop; on the generic path it lives inside the default body's per-impl instantiation. There is no third path, so struct-copy aliasing cannot occur. And correctness never depends on the inliner firing — the recursion guard and occurrence-check fallback both emit real calls in some cases.

**Defaults must be overridable.** `Generator<a>` is itself an `Iterable` and its `as-generator` is the identity; it must override rather than wrap itself in another generator. Any .NET-backed sequence does the same. This is also the escape hatch for an impl whose cursor cannot survive the default body.

**Settle first:** whether the default body can be written in Bjolang depends on how `Generator` compiles. If it becomes a C# iterator method, `yield` inherits C#'s restrictions — none inside a lambda, none in a `try` with a `catch` — and a loop in the default body lowers to `while(true) switch` with `goto case`. Confirm `yield` survives that; the fallback is one runtime extern taking four delegates.

**Disposal:** an enumerator-backed generator holding a file handle leaks if nothing disposes it. Whatever consumes a `Generator` emits the `try/finally`, as C#'s `foreach` does. The inlined cursor path needs nothing.

---

## Part 3 — The `Collector` trait

```
(trait Collector (c) (Elem) (Acc) (Out)
  (init   : (c -> Acc))
  (step   : (c -> Acc -> Elem -> Acc))
  (finish : (c -> Acc -> Out)))
```

First-order with associated types, and it needs **no bridge**: `init`/`step`/`finish` pass and return values with no cursor aliasing, so the generic path is the ordinary interface path.

`folding` is the **primitive**: `(folding seed expr)`. `listing`, `vectoring`, `summing`, `counting`, `maximizing`, `minimizing`, `appending`, and `into` are the stock set.

### Argument convention

**The last positional argument is the per-iteration step expression; everything before it, plus every keyword, is a construction argument.**

```
(listing b)               ; step = b
(vectoring x #:length n)  ; step = x,    construction = #:length n
(folding #f test)         ; step = test, construction = #f
```

A convention of the desugarer, not something `Collector` enforces, so a collector taking two per-iteration values cannot express that. Accepted for now.

> **Later:** per-impl keyword arguments would remove the convention. That is the same shape as the problem constructor variables solved — a trait method whose signature varies per impl has nothing the trait can declare — and since collectors are inline-only, the resolved impl is known at every use site. It falls out of the inline-trait machinery once that lands; it is impossible before.

---

## Part 4 — Clause language

### Shape

A flat clause list, optionally preceded by a loop name, optionally ending in `=>` and a result expression. There is no body position.

```
(loop (:for x xs)
      (:let y (expensive-1 x))
      (:when (test? y))
      (:let z (expensive-2 y))
      (:acc out (listing z))
      => out)
```

### Levels

Levels are **implicit**, determined by `:for` adjacency:

> A `:for` clause preceded by anything other than another `:for` clause opens a new level.

Consecutive `:for` clauses therefore advance in **lockstep** at the same level, and that level ends when the shortest is exhausted. Every non-`:for` clause belongs to the innermost level open at its position.

No clause "opens a level" as its own effect. `:subloop` exists solely because it is *not* a `:for`, so placing it between two `:for` clauses forces the second into a new level. That is exactly why it is `(:when #t)` — a filter that always passes, whose only observable role is to break the adjacency.

```
;; lockstep — pairs up
(loop (:for k keys) (:for v vals) (:acc pairs (listing (tuple k v))))

;; nested — cartesian product, forced by :subloop breaking adjacency
(loop (:for x xs) (:subloop) (:for y ys) (:acc out (listing (tuple x y))))

;; also nested — :let breaks adjacency just as well
(loop (:for x xs) (:let n (size x)) (:for y (children x)) …)
```

Note the third case: a `:let` between two `:for`s makes the second a subloop. This is the rule doing its job — the inner sequence usually depends on the binding — but it means clause order alone determines nesting, with no marker at the nesting site.

Level assignment is a single left-to-right pass: track whether the previous clause was a `:for`, increment the level counter when a `:for` follows a non-`:for`, and assign every other clause to the current level.

### Clauses

| Clause | Meaning |
|---|---|
| `(:for pat seq)` | Bind `pat` to successive elements of `seq` at the current level. `pat` may destructure. |
| `(:let pat expr)` | Per-iteration binding, evaluated in clause order. Not a monadic bind. |
| `(:do expr …)` | Arbitrary imperative code, evaluated for effect at this point. Value discarded. |
| `(:when cond)` | Skip the rest of this iteration of the **current** level unless `cond` holds. A filter. |
| `(:subloop)` | No effect of its own. Exactly `(:when #t)`. Its purpose is to break `:for` adjacency and force a new level. |
| `(:acc name (collector …) #:when cond)` | Declare an accumulator. `#:when` is optional and gates only the step. |
| `(:break cond)` | Terminate the **entire** loop when `cond` holds, before the rest of the iteration runs. Routes through finish. |
| `(:end-subloop-if cond)` | Abandon the **current subloop** when `cond` holds and resume the enclosing level's next iteration. An error at level 0. |
| `(:final cond)` | Terminate after the current iteration completes. Desugars — see below. |

`#:when` on `:acc` is a **clause modifier intercepted by the loop form**, never passed to the collector, because it references loop variables while construction arguments are hoisted.

### `:when` and `:end-subloop-if` are different jumps

`:when` skips one iteration of the level it appears in. `:end-subloop-if` terminates that level entirely and resumes the enclosing one — it is an early return from a subloop, not an iteration skip. In the edge table of 5c, `:when` false jumps to `M_i`; `:end-subloop-if` jumps to `M_{i-1}` with the parent's cursors advanced, the same edge as inner-level exhaustion.

At level 0 there is no enclosing level, which is why the two coincide there. That is a coincidence, not a definition: **`:end-subloop-if` at level 0 is an error**, and the message should name the two clauses the author probably wanted — `:when` to filter, `:break` to leave the loop.

**Clauses do not compose.** Each carries its own condition rather than being guarded by a preceding `:when`; there is no bare `(:break)` to be conditionally reached. (For consistency `:break` and `:final` also take conditions, though they do not carry the `-if` suffix.)

Clause order is significant: a `:when` between two `:let`s means the second does not evaluate on skipped iterations.

### Clause keywords are clauses only

Every keyword in the table above is recognized **only at the top level of the clause list**. Inside a `:do` body they are not control flow and carry no meaning. `:break`, `:end-subloop-if`, and `:final` in particular cannot be reached from imperative code — doing so would require them to be expressions with a jump target, which is a separate feature.

Silently ignoring them is the wrong failure mode, and so is letting them fall through to an unbound-variable error from a confusing position. The desugarer already knows the keyword set, so when walking a `:do` body it checks for any of them in head position and reports: *"`:break` is a clause, not an expression — move it out of the `:do`."*

That check must **not** descend into a nested `loop` form inside the `:do`. An inner loop's clauses belong to the inner loop and are handled by its own desugaring.

### `:final` is a rewrite, not a mechanism

```
(:final test)   ⇒   (:break tmp)
                    (:acc tmp (folding #f test))
```

Both replacements sit at the position `:final` occupied, **`:break` first**. This works because an accumulator slot read at the top of iteration *N* holds what was written at the end of iteration *N−1*, so the break sees the previous iteration's verdict — exactly "finish this one, then stop." Because accumulators are hoisted and live as slots on every member, a subloop returns to the enclosing level with the flag carried through untouched, so no separate control state is needed.

- **Order is load-bearing.** Reversed, the break reads the value written this iteration and `:final` collapses into `:break`.
- **`tmp` is a gensym** the alpha-renamer treats as any other binder and the user cannot name.
- Clauses before the `:final` position still run on the terminating iteration, inherited from `:break`.
- The `#f` seed pins `Elem = Bool` from a construction argument, so the generated accumulator can never hit the unpinned-`Elem` ambiguity of 5f.

### Accumulators are hoisted and never reset

Every accumulator's collector value and initial accumulator are bound in the prologue and are in scope for the whole loop, including inside subloops and **including the finish block**. An accumulator declared inside a subloop **persists across outer iterations**; it does not reset on re-entry. Code wanting per-entry reset uses a mutable variable and does it by hand. There is no syntax for it and none is planned.

### The finish block

Every exit runs it. Nothing bypasses it.

`=> expr` is evaluated with each accumulator in scope **by name, bound to its finished value**. Loop variables are not in scope — they do not exist at exit.

With no `=>`: auto-generate. A tuple of the finished accumulators in declaration order, the single accumulator if there is exactly one, and **void** if there are none — a loop with no accumulators and no `=>` is pure effect and has nothing to deliver.

```
(defun partition (xs pred)
  (loop (:for x xs)
        (:acc yes (listing x) #:when (pred x))
        (:acc no  (listing x) #:when (not (pred x)))))
;; => (tuple yes no)
```

### Named loops — tail position only

`(lp)` advances every clause; `(lp #:a expr)` overrides `a` and advances the rest. Bjolang has keyword arguments, so goof-loop's `(lp (=> a expr))` becomes a keyword call and the non-hygienic name comparison is unnecessary.

**In a named loop, the final `:do` takes over the continue edge.** If it tail-calls `lp`, that is the jump. If it completes without calling `lp`, the loop exits — through the finish block, like every other exit.

```
(loop lp (:for a (range 0 100))
         (:acc out (listing a))
         (:do (if (special? a) (lp #:a (+ a 10)) (lp)))
         => out)
```

**The loop name may appear only in tail position** within that final `:do`. A `TRecur` is a jump, not a call, so a non-tail use has no lowering — and neither does using `lp` as a value. Both are errors, reported at the offending occurrence, saying the loop name must be tail-called. This is a real restriction relative to Scheme, where `(cons a (lp …))` builds its result on the way out; in Bjolang that shape is written with an accumulator.

---

## Part 5 — Compilation

### 5a. Prologue

Everything loop-invariant is bound **outside** the loop. Build the prologue as an ordered list of `(name, expr)` that clause translation appends to, and emit it around the finished `TLoop`.

- Per `:for`: the **evaluated sequence** `s_i`, evaluated exactly once, never per iteration.
- Per level-0 `:for`: the **initial cursor** `(start s_i)`.
- Per `:acc`: the **collector value** built from its construction arguments, and the **initial accumulator** `(init c_j)`.

`start` and `init` are trait calls at a resolved type, so they inline in the prologue and cost nothing beyond the values they produce.

**Hoisting is per-clause, not unconditional.** An inner level's `start` re-runs on each entry to that subloop, so it belongs at the entering jump. An inner sequence expression depending on an outer loop variable — `(:for y (children x))` — belongs in the enclosing level's body. Only genuinely invariant expressions move.

**Construction arguments must be loop-invariant.** They are evaluated before any loop variable exists. `(vectoring y #:length n)` is fine if `n` is invariant and an error if `n` came from a `:for`. Scan for loop-variable references and give a real error rather than an unbound-variable failure from a confusing position.

### 5b. Prologue bindings must not generalize

`(listing)` is a syntactic value, so `isValue` returns true, `generalize` fires, and the binding gets `∀a. Listing<a>`. Every `step` inside the loop then instantiates a *fresh* `a`, the element type never pins, and `finish` produces an `Out` that is ambiguous or silently wrong. **Prologue bindings are monomorphic.**

Cheapest route with no new AST: desugar the prologue as immediately-applied lambdas — parameters bind monomorphically, and guaranteed beta on direct application makes it free. Since later bindings reference earlier ones that means nesting; a non-generalizing flag on `TLet` is probably cleaner for a chain this long.

Two things then work in your favour, both contingent on this fix. `ResolveAssociatedType` keys on the head constructor, and `Listing` is known in the prologue even with the element type an open meta, so `Acc` and `Out` resolve immediately rather than blocking on the loop body. And because the binding sits in the environment as a monotype while the body is inferred, its meta is free in the environment and cannot be prematurely generalized by an inner `let`.

### 5c. One flat `TLoop` group

`TLoop of TLoopMember list * TypedExpr option` already models a group of mutually tail-recursive members, `TRecur of int * TypedExpr list` jumps to member *i* with a complete argument vector aligned with that member's `Slots`, and `Codegen.generateMergedLoop` emits the group as `while(true) switch(state)` with `goto case N` across members and `continue` for self-jumps.

**Emit all levels as members of a single group. Do not nest groups.** `goto case` binds to the *nearest* enclosing switch, so nested groups are exactly where a cross-level jump would silently bind to the wrong one.

Members: `M_0 … M_n`, one per level, plus `M_exit` — the finish block, which applies `finish` to every accumulator, binds each by name, and evaluates the `=>` expression.

`M_i`'s slots: the cursors of level *i*'s `:for` clauses, plus **every** accumulator (hoisted, in scope throughout, and needed by `M_exit`).

| Situation | Jump |
|---|---|
| `M_i` exhausted, *i* > 0 | `M_{i-1}`, parent cursors advanced |
| `M_0` exhausted | `M_exit` |
| `M_i` not exhausted, *i* < *n* | `M_{i+1}`, its cursors freshly `start`ed |
| `M_n` iteration complete | `M_n`, cursors advanced, accumulators stepped |
| `:when` false | `M_i`, cursors advanced |
| `:end-subloop-if` fires, *i* > 0 | `M_{i-1}`, parent cursors advanced |
| `:break` fires | `M_exit` |
| named loop's final `:do` completes without calling `lp` | `M_exit` |

No clause needs threaded control state. `:break`, `:when`, and `:end-subloop-if` are jump targets; `:final` reduces to `:break` plus an accumulator.

### 5d. Slot vectors

`TRecur` carries a **complete** argument vector, so a named-loop partial update `(lp #:a expr)` must be expanded at desugar time into a full vector reading the other slots. `Locals` gives the per-iteration copies, parallel by index with `Slots`.

### 5e. Ordering

Parse and desugar → inference → trait inlining → dictionary lowering → `LoopLowering` → alpha-rename → codegen. Inlining **must** precede `LoopLowering`: `TRecur` carries an index into its enclosing `TLoop`, so splicing a body containing one afterwards produces a silently wrong jump.

**On emitting the group.** `TLoop`/`TRecur` are typed nodes, so emitting them directly requires the loop form to be typechecked natively — inference would need to understand clauses. Desugaring to a `letrec` of lambdas before inference avoids that entirely, and the shape is fully under the desugarer's control: every level is a lambda, every level transition is a tail call by construction, so there is nothing "advanced" for promotion to fail on beyond handling mutual groups at all.

The risk is not that the shape is inexpressible but that promotion silently *declines*, leaving real closures and real calls — correct, but allocating per level entry and unable to iterate deeply without overflowing. **Do not leave that to chance.** Mark the generated group as loop-derived and add an assertion pass after `LoopLowering`: if a marked group did not become a `TLoop`, fail the compile. A desugaring bug then surfaces in the test suite rather than as a mysterious stack overflow in user code.

If that assertion proves impossible to satisfy, the fallback is native typechecking of `loop` with direct `TLoop` emission — more control, at the cost of building `Slots`/`Locals`/`TRecur` alignment by hand and teaching inference about clauses.

### 5f. Typing

- Each `:for` yields `ResolveAssociatedType Iterable Elem seqType` for the pattern and `… Cursor …` for the slot.
- Each `:acc` yields `Acc` for the slot and `Out` for its binding in the finish block.
- `(loop (:acc out (listing x)))` with nothing pinning `Elem` is ambiguous — a real error with a range on the `:acc` clause, not a unification failure.

---

## Part 6 — Performance

The goal is that `(loop (:for i (range 0 n)) …)` matches a hand-written `for`. After inlining it already has the right shape — `c = lo`, `c < hi`, `c += step`. What remains is data representation and codegen.

**`loop` is an expression, but C#'s `while` is a statement.** In expression position the loop must still lower to jumps, not to C# local functions — `TLoop(members, Some body)` binding members as callable locals would turn every level transition into a call and lose tail recursion. Use the existing hoisting mechanism: emit the loop as statements assigning a temporary, then use the temporary.

**Cursors must be value types.** `next` returns a new cursor every iteration, so a reference-type cursor costs one allocation per element. `TTuple` already emits `ValueTuple<…>`, so composed cursors — `indexed`'s `(Int, InnerCursor)` — are allocation-free provided their leaves are.

**User types are currently reference types.** `TType` emits `public record Name(…)`, a record *class*. So `Range` is heap-allocated and `s.hi` is a load through a reference each iteration, which RyuJIT's escape analysis will not reliably promote. Add `record struct` support — a `#:struct` marker, or auto-derive for small non-recursive single-constructor immutable records. Then `s` is a local, its fields promote to locals, and literal bounds constant-fold. This is the single biggest lever for `range` and for any composed cursor with a user-defined leaf.

**`done?` on `Vec` must read the array's `.Length` directly**, and `current` must index the same array expression. RyuJIT's bounds-check elimination pattern-matches `i < arr.Length` against `arr[i]`; a cached `_count` field defeats it and costs a check per element.

**Do not merge single-level loops.** `TLoop([m], None)` emits a plain `while`; the merged form emits `while(true) switch(state)`, putting a dispatch in every loop in the language. A loop with `:break` needs `M_exit` as a jump target, which forces the merged form — so add a codegen peephole: a two-member group where `M_exit` is entered only from `M_0`'s exit edges lowers to a plain `while` with C# `break`, followed by the finish code. A false `:when` in a single-level loop is C# `continue`.

**Verify field access is a direct read.** `(range-hi s)` on a single-constructor record should emit `s.hi`, not a type test or match.

The last stretch depends on RyuJIT doing struct promotion and constant folding rather than on anything emitted — measure one case against a hand-written `for` with BenchmarkDotNet before building on top of it.

---

## Part 7 — Extensibility

- **A new sequence** is an `Iterable` impl. It works on both paths automatically, because `as-generator` has a default body.
- **A new accumulator** is a `Collector` impl, subject to the Part 3 argument convention.
- **Clause keywords are fixed.** No `register-loop-clause` analogue until hygienic macros exist. Note it as a known gap.

---

## Part 8 — Prerequisites

1. **Default method bodies in traits.** A default body must be instantiated per-impl at that impl's target type, resolving `start`/`done?`/`current`/`next` against the impl it is used for. That is the existing `InlineTemplate` mechanism keyed on the trait declaration rather than an impl. Worth having independently — `!=` from `==`, `>=` from `<=`.
2. **`ResolveAssociatedType` ignores the receiver's type arguments.** It keys on the head constructor name and returns whatever the impl stored, so `Cursor` for `impl Iterable for (List 'a)` comes back as an unsubstituted `TVar "'a"`. Both associated types go through it.
3. **`Implementations` discards impl target type arguments.** Same fix — `ImplTarget` patterns.
4. **Keyword argument naming.** Codegen emits `sanitizeIdent kwName + ":"` at the call site but declares the parameter `__kw_{sanitizeIdent kwName}`. Named-loop partial update depends on this.
5. **Confirm multi-member loop groups actually form.** `TLoop([m], None)` has a fast path emitting a plain `while`; `generateMergedLoop` handles the rest. Verify with three mutually recursive local functions, each tail-calling the next, that the emitted C# is one `switch` with no method calls. The flattening strategy rests on this.
6. **A non-generalizing binding form**, for 5b.
7. **`record struct` support**, for Part 6. Not blocking for correctness; blocking for the performance claim.

---

## Open decisions

- **A `:with`-style clause** for user-supplied loop-invariant bindings, now that the prologue is first-class.

---

## Tests

- `:for` over a list and a vector at concrete types — emitted C# is a plain `while`, no per-iteration allocation, no `switch`.
- A loop in expression position — lowers to jumps, not local functions; deep iteration does not overflow the stack.
- The same loop inside a function generic over its sequence — routes through `as-generator`, and is *correct*, which is the point.
- A generator with a `finally` — the `finally` runs, including on `:break`.
- Lockstep `:for` over different-length sequences — terminates at the shortest, and the longer one's `current` is never called past its end.
- The sequence expression in `(:for x (expensive))` is evaluated exactly once; so is a collector's construction argument.
- A construction argument referencing a loop variable — the loop-invariance error, not an unbound-variable failure.
- **Two loops in one function, both using `(listing)`, at different element types** — the 5b generalization regression test.
- `:subloop` producing a cartesian product; accumulators declared outside collect across all inner iterations; one declared *inside* does not reset on re-entry; an inner `:for` depending on the outer variable is not hoisted.
- `(:for x xs) (:let n …) (:for y …)` — the second `:for` is a subloop, not a lockstep partner. Adjacency rule regression test.
- Three consecutive `:for` clauses — all lockstep at one level, one member emitted.
- A loop with no accumulators and no `=>` — types as void.
- Every generated loop group promotes to a `TLoop` — the assertion pass fires on a deliberately broken desugaring.
- `:when` between two `:let`s — the second does not evaluate on skipped iterations.
- `:break` from inside a subloop — exits the whole loop and runs finish.
- `:end-subloop-if` from inside a subloop — abandons that level and resumes the parent's next iteration, not the same level's.
- `:end-subloop-if` at level 0 — the error, naming `:when` and `:break` as alternatives.
- `:break` versus `:final` on the same predicate — differ by exactly one iteration's worth of accumulation.
- `:final` inside a subloop — the subloop completes before termination.
- `:final` where a user accumulator is named `tmp` — no capture.
- Multiple `:acc` with no `=>` — auto-generated tuple; and the `partition` example with `#:when`.
- Named loop whose final `:do` declines to call `lp` — exits through finish with accumulators finished, not raw.
- Named loop with `(lp #:a expr)`; and `lp` in non-tail position → the tail-position error.
- `(:do (:break done?))` → the "clause, not an expression" error, not a no-op and not an unbound variable.
- A nested `loop` inside a `:do`, with its own `:break` — the inner clauses are not flagged.
- `(loop (:acc out (listing x)))` with unpinned `Elem` — the ambiguity error.
- A user-defined `Iterable` impl in a separate module, consumed across a `.dll` boundary.
- `Generator`'s own `Iterable` impl overriding `as-generator` with the identity — no double wrapping.
