# Ports for Bjolang — design spec

> **Status.** Phase 1 (`char`) and phase 2 (text ports) are implemented, in
> `lib/std/ports.bjo` and `TestFiles/68_ports.bjo`. Phase 6 (`#:port` on
> `display` and friends) is **dropped** for now: without dynamic binding it is
> only an explicit argument, and not worth the surface.
>
> Four things were learned by building it that the design below did not
> anticipate. They are recorded in "What implementing it changed" at the bottom.

An R7RS-shaped port system, adapted to a statically typed language with
Hindley–Milner inference, `Option`/`Result`, keyword arguments and `with-open`.

Nothing here is implemented yet. This is the map.

## What already exists

| Thing | Where | Note |
| --- | --- | --- |
| `display`, `displayln`, `newline`, `read-line` | `Prelude.fs:75-79` | console only, no port argument |
| `open-text-reader`, `open-text-writer` | `Prelude.fs:94-95` | already return `System.IO.TextReader` / `TextWriter` |
| `writer-write-line`, `writer-flush`, `close-handle` | `Prelude.fs:98-100` | an ad-hoc first stab at this system |
| `with-open` | `Parser.fs:1073` | binds, then `.Dispose` in a `finally`, one try per binding |
| `import/class`, `import/extern`, `#:exceptions` | `52_`/`54_dotnet_*.bjo` | declared exceptions become `(Result System.Exception %a)` |
| Keyword arguments with expression defaults | `13_naughty_kwarg.bjo` | see "Why keyword arguments matter" below |
| `Option`, `Result` | `Prelude.fs:185-202` | |
| `Seq` + `seq-unfold` | `Prelude.fs:206-221` | lazy, re-enumerable |
| `byte` | `TypedAST.fs:86` | |
| `BjoChar` | `BjolangRuntime/BjoChar.cs` | **exists but is not hooked into the compiler** |

The existing `open-text-reader`/`writer-*`/`close-handle` group should be
considered this system's prototype and be replaced by it, not left alongside.

---

## Decision 1 — a port *is* the .NET object

`TextReader` is already the abstraction over "file, string, or console input";
`TextWriter` over output; `Stream` over bytes. .NET has done the work.

So a Bjolang port is a thin alias for the .NET type, brought in with
`import/class`. No wrapper record, no runtime tag, no new runtime code.

Two things fall out for free:

- **`with-open` works unchanged.** It emits `(.Dispose p)`, and every one of
  these is `IDisposable`. A port wrapped in a Bjolang record would need
  `with-open` extended to know about ports.
- **Peek needs no buffer.** `TextReader.Peek()` already exists, so `peek-char`
  is not forced to invent a one-character pushback buffer.

The cost: a port cannot carry Bjolang-level state — no line counter, no
user-defined ports written in Bjolang. See "Deferred: user-defined ports".

## Decision 2 — four port types, statically distinguished

This is the main departure from R7RS, and the main win.

```
                 text                 binary
input     TextInputPort          BinaryInputPort
output    TextOutputPort         BinaryOutputPort
```

backed by `System.IO.TextReader`, `TextWriter`, and `Stream` twice.

R7RS has one `port?` and finds out at runtime that you called `read-u8` on a
textual port. Here that is a type error, and `input-port?` / `textual-port?`
mostly stop needing to exist. Keep `port?`-style predicates out of the design
until something actually needs them.

**The direction split is worth its weight; the text/binary split is unavoidable**
(different element types). Note that input and output are genuinely different
.NET types, so this costs nothing to enforce.

## Decision 3 — EOF is `Option`, not an eof-object

```scheme
(: read-line (-> TextInputPort (#:keep-newline bool) (Option string)))
(: read-char (-> TextInputPort (Option char)))
(: read-byte (-> BinaryInputPort (Option byte)))
```

R7RS returns a distinguished eof object that is a value of the same type as a
character, which only works because it is dynamically typed. `Option` is the
honest encoding and it composes with everything already in the prelude.

`eof-object?` does not exist. `None` is the eof object.

## Decision 4 — opening returns `Result`, operating throws

Two failure classes, treated differently on purpose:

- **Opening** — missing file, bad permissions, path too long. Expected, and the
  caller usually has something sensible to do. → `Result`, via `#:exceptions`.
- **Operating on an already-open port** — disk full, device error. Rare and
  usually unrecoverable, and threading a `Result` through every `read-char`
  would poison the whole API. → let it throw, per the existing rule that "an
  exception nobody named is a bug rather than a result."

So:

```scheme
(: open-input-file  (-> string ... (Result System.Exception TextInputPort)))
(: read-line        (-> TextInputPort ... (Option string)))
```

`read-line` returning `(Result e (Option string))` would be the alternative, and
it is bad enough at the call site to settle the argument.

**EOF is not an error.** That is what keeps `read-line` a one-level `match`.

---

## Why keyword arguments matter here

You asked whether keyword arguments buy a more powerful interface. They do, and
more than they would in most languages — but only for one job.

`13_naughty_kwarg.bjo` shows the capability is unusually strong:

```scheme
(: f (-> int (#:acc int) (#:other int) int))
(defun (f n #:acc (let loop ((i 3) (a 0)) (if (= i 0) a (loop (- i 1) (+ a 1))))
           #:other (+ acc 100))
  (+ (+ n acc) other))
```

A default may be an **arbitrary expression**, and a later default may **read an
earlier parameter**. These are not C# optional parameters, which must be
compile-time constants. For a port API this means a default can be computed from
the port or from another option.

### The rule: keywords configure, they never dispatch

> A keyword argument may tune *how* an operation is performed. It may not decide
> *which* operation is performed, and it may not change the return type.

The moment a flag changes the return type, the signature cannot express it —
the return type is fixed. The moment it changes the meaning, two names are
clearer than one name and a boolean.

### Where they earn their place

**Opening a file.** This is the strongest case. Without keywords you get a
combinatorial explosion of names or a config record.

```scheme
(: open-input-file
   (-> string
       (#:encoding Keyword)          ; :utf8 :utf16 :ascii :latin1
       (#:buffer-size int)
       (Result System.Exception TextInputPort)))

(: open-output-file
   (-> string
       (#:encoding Keyword)
       (#:if-exists Keyword)         ; :error :truncate :append
       (#:if-missing Keyword)        ; :error :create
       (#:buffer-size int)
       (Result System.Exception TextOutputPort)))
```

`#:if-exists` is worth calling out: R7RS has no answer for it and everyone
reimplements it badly. Common Lisp got this right and it maps cleanly onto .NET
`FileMode`.

**Reading with a bound or a variation.**

```scheme
(: read-line   (-> TextInputPort (#:keep-newline bool) (Option string)))
(: read-string (-> TextInputPort (#:max int) (Option string)))
(: port-lines  (-> TextInputPort (#:trim bool) (Seq string)))
```

**Slices on write**, where R7RS uses positional `start`/`end` arguments that
nobody can remember the order of:

```scheme
(: write-string (-> TextOutputPort string (#:start int) (#:end int) void))
```

**An explicit current port**, which is strictly better than R7RS here. R7RS
rebinds `current-output-port` dynamically; Bjolang has no dynamic binding, and
does not need it:

```scheme
(: displayln (-> string (#:port TextOutputPort) void))
;; default: (current-output-port)
```

This keeps every existing `(displayln "x")` call working unchanged while making
the port explicit and statically typed when you want it. It is the one place
where the "arbitrary expression default" capability is load-bearing.

### Where they would be wrong

| Tempting | Why not |
| --- | --- |
| `(open-file path #:binary #t)` | Changes the return *type*. Impossible, and it is what Decision 2 is for. |
| `(read-char port #:peek #t)` | Changes the operation, not a parameter of it. `peek-char` is a separate function. |
| `(read-line port #:on-error :result)` | Changes the return type. Impossible. If both behaviours are wanted, that is two functions. |
| `(open-input-file path #:mode :output)` | Direction is a type, not a flag. |

The first and third are the useful boundary: **a keyword cannot change a
return type**, so any option that would is telling you it should have been a
type distinction or a second function.

---

## Proposed surface

### Types

```scheme
TextInputPort  TextOutputPort  BinaryInputPort  BinaryOutputPort
```

### Opening and closing

```scheme
(: open-input-file   (-> string (#:encoding Keyword) (#:buffer-size int)
                         (Result System.Exception TextInputPort)))
(: open-output-file  (-> string (#:encoding Keyword) (#:if-exists Keyword)
                         (#:if-missing Keyword) (#:buffer-size int)
                         (Result System.Exception TextOutputPort)))
(: open-binary-input-file  (-> string (Result System.Exception BinaryInputPort)))
(: open-binary-output-file (-> string (#:if-exists Keyword)
                               (Result System.Exception BinaryOutputPort)))

(: close-port (-> %p void))     ; see "Open question: closing generically"
(: flush-port (-> TextOutputPort void))
```

`with-open` already covers the scoped case and needs no new form:

```scheme
(match (open-input-file "data.txt")
  ((Ok port) (with-open ((p port)) (read-all-lines p)))
  ((Err e)   (handle e)))
```

### String ports

```scheme
(: open-input-string (-> string TextInputPort))

;; The primary form: the string is the result, so nothing has to be extracted
;; from a port afterwards.
(: call-with-output-string (-> (-> TextOutputPort void) string))
```

`call-with-output-string` is the answer to a real typing problem — see
"Open question: string output ports".

### Textual input

```scheme
(: read-char   (-> TextInputPort (Option char)))
(: peek-char   (-> TextInputPort (Option char)))
(: read-line   (-> TextInputPort (#:keep-newline bool) (Option string)))
(: read-string (-> TextInputPort (#:max int) (Option string)))
(: read-all    (-> TextInputPort string))
(: port-lines  (-> TextInputPort (#:trim bool) (Seq string)))
```

`port-lines` returning a `Seq` is the idiomatic win over R7RS: `seq-filter`,
`seq-map` and `seq-take` all apply, and it is lazy, so a large file streams.
Build it with `seq-unfold` over `read-line`.

### Textual output

```scheme
(: write-char   (-> TextOutputPort char void))
(: write-string (-> TextOutputPort string (#:start int) (#:end int) void))
(: write-line   (-> TextOutputPort string void))
(: newline      (-> (#:port TextOutputPort) void))
```

### Binary

```scheme
(: read-byte   (-> BinaryInputPort (Option byte)))
(: read-bytes  (-> BinaryInputPort int (Option (Array byte))))
(: write-byte  (-> BinaryOutputPort byte void))
(: write-bytes (-> BinaryOutputPort (Array byte) (#:start int) (#:end int) void))
(: port-bytes  (-> BinaryInputPort (Seq byte)))
```

### Standard ports

```scheme
(: current-input-port  (-> TextInputPort))
(: current-output-port (-> TextOutputPort))
(: current-error-port  (-> TextOutputPort))
```

Functions, not values, so they dodge the module-level open-type restriction and
can be used as keyword defaults.

---

## Prerequisites

1. **Hook up `BjoChar`.** `read-char`, `peek-char` and `write-char` are core, and
   the runtime type exists but the compiler does not know it. Needs a
   `charType`, literal syntax (`#\a`, `#\newline`, `#\x41`), equality, and
   `char->int` / `int->char` / `char->string`. Until then the textual API stops
   at `read-line`/`read-string`, which is a usable but incomplete port system.

   *Alternative if you want to defer:* have `read-char` return
   `(Option string)` holding a one-character string. Cheap, works today, and
   wrong in the long run — it makes `char` and `string` the same type at exactly
   the place the distinction matters.

2. **Decide the encoding representation.** `#:encoding Keyword` (`:utf8`) is
   cheap and needs nothing new, but is unchecked — `:utf-8` typos silently. An
   `Encoding` union would be checked. Keyword is the pragmatic start.

## Open questions

**Closing generically.** `close-port` wants to accept all four types. HM has no
subtyping, so it is either four functions (`close-input-port`,
`close-output-port`, …), a trait, or `close-handle`'s existing trick of typing
the argument as `System.IDisposable` and relying on the CLR. The last one works
today and is what `Prelude.fs:100` already does. Probably: keep it, and note
that `with-open` makes explicit closing rare anyway.

**String output ports.** `get-output-string` is the wrinkle. If
`TextOutputPort` = `TextWriter`, then `get-output-string` type-checks against
*any* output port but only works on a `StringWriter`, and a file port would fail
at runtime. Three ways out:

- `call-with-output-string` only — the string is the return value, so nothing is
  extracted. Type-safe, and covers most uses. **Recommended.**
- `(: get-output-string (-> TextOutputPort (Option string)))` — honest about the
  runtime check, but a weak type.
- `open-output-string` returns a record of the port plus a `(-> string)` getter
  closure. Type-safe and supports incremental use, at the cost of a second
  concept.

I would ship the first and add the third only if incremental building turns out
to be needed.

**Keyword arguments on trait methods.** If ports later become a trait
(see below), it is not established that trait methods may carry keyword
parameters — `FunMeta` is name-keyed and trait dispatch is a separate path.
Worth testing before designing on it.

## Deferred: user-defined ports

A `Port` trait — so a user could implement a port over a socket, a compression
stream, or a test double — is the obvious extension, and Decision 1 rules it out
for now by making a port a .NET object rather than a Bjolang value.

That is the right trade for a first version: .NET's `TextReader`/`Stream`
hierarchy already covers file, string, console, network and memory, which is
everything an early user actually wants. Revisit when someone needs a port whose
behaviour is written in Bjolang.

If it is revisited, the shape is a trait with an associated type, in the style
of `Foldable`:

```scheme
(def/trait (TextInput %p)
  (: read-char-impl (-> %p (Option char)))
  (: close-impl     (-> %p void)))
```

and the concrete types become implementations of it.

## What implementing it changed

Four things, all found by probing rather than by reasoning. Each is a fact
about the language that the design above got wrong or did not know.

**1. A declared base-class return type is rejected, not widened.** The hope was
that an extern could be declared to return `TextReader` even though
`System.IO.File.OpenText` returns `StreamReader`, letting C#'s implicit upcast
do the work. It does not: the compiler checks the declaration against the CLR
signature and fails with *Cannot unify TextReader with StreamReader*. Hindley–
Milner has no subtyping and the interop layer does not pretend otherwise.

**2. `cast` performs an upcast, and that is what unifies the port kinds.**
`(cast TextInputPort (StringReader. s))` compiles and runs. So the concrete
readers are upcast at the door, inside `open-*`, and a file port and a string
port genuinely are the same type to every caller. This is what makes
`(: count-lines (-> TextInputPort int))` work over both.

**3. Traits over imported CLR classes work, including generic dispatch.**
Probe-verified: `(def/impl (TextIn StringReader) ...)` and
`(def/impl (TextIn StreamReader) ...)` with a generic `(: first-line (-> %p string))`
compiles and dispatches correctly. So the deferred "user-defined ports" trait is
a real option whenever it is wanted — but it is *not needed* for file-versus-
string, which decisions 1 and 2 already settle at zero dictionary cost. Traits
are the right tool for a port whose behaviour is written in Bjolang; they would
be overhead here.

**4. An `import/class` alias is module-local and cannot be published.** There is
no export mechanism for it — `Program.fs` exports values, traits, types and
externs, but not `ClrClasses`. Importing `System.IO.TextReader` *as*
`TextInputPort` therefore leaves every caller unable to name the type. The fix
is a **type alias**, which is exported:

```scheme
(type
  (: TextInputPort System.IO.TextReader)
  (: TextOutputPort System.IO.TextWriter))
```

with one trap: an exported alias is serialized as whatever it points *at*, so
pointing it at the module's own `import/class` alias exports a name the caller
cannot resolve either. It has to name the fully qualified .NET type.

### Two decisions settled while building

**Closing is two functions, not one.** `.Dispose` needs a concrete .NET
receiver, so a generic `(: close-port (-> %p void))` fails with *its target has
the Bjolang type %p, which is not a .NET class*. `close-input-port` and
`close-output-port` it is. The `System.IDisposable` alternative only moves the
problem to a cast at every call site, and `with-open` makes explicit closing
rare regardless.

**EOF: `Option` for lines, `port-eof?` for characters.** The instinct that a
manual check is the faster interface is right, but the cost is per-*character*,
not per-line: an `Option` allocation is free next to the allocation of the line
it wraps. So `read-line` returns `(Option string)` and character-level reading
will use `port-eof?` when it lands.

There is a second reason `read-line` cannot simply return a string: .NET's
`ReadLine` returns **null** at end of input, and Bjolang has no null to test
against. An unguarded call would hand a null string to the rest of the program.
`read-line` asks `port-eof?` first rather than trying to detect it afterwards.

The open sub-question — what a character-level `read-char` returns *at* EOF —
is still open. Erroring, with `port-eof?` as the documented precondition, keeps
the sentinel out of the type; returning `#\null` reintroduces R7RS's ambiguity
for real data.

## Suggested phasing

1. ~~`BjoChar` hooked up, with literals and conversions.~~ **Done.**
2. ~~Types + opening/closing + `read-line`/`write-string`/`read-all`.~~ **Done.**
3. `port-lines` as a `Seq`, and `call-with-output-string`.
4. Character-level textual I/O. Note that `TextReader.Read` returns a UTF-16
   *code unit*, so `read-char` has to combine a surrogate pair itself to produce
   a `char` — the same problem the lexer had with `#\😀`.
5. Binary ports.
6. ~~The `#:port` keyword on `display`/`displayln`/`newline`.~~ **Dropped**
   pending dynamic binding.

### Still to do from phase 2

- `open-output-string` / `call-with-output-string`. Deliberately left out of the
  first cut because of the typing wrinkle in "Open questions" — worth settling
  before adding it rather than after.
- `#:encoding`, `#:if-exists` and the other opening keywords. The functions
  exist with their mandatory arguments only; the keywords are additive.
- Replacing the ad-hoc `open-text-reader` / `writer-write-line` /
  `close-handle` group in `Prelude.fs:94-100`, which this system supersedes.

## Fixes needed in the current `ports.protobjo`

The file does not parse. Against the working form in `52_dotnet_class_io.bjo`:

```scheme
(import/class
  (StreamWriter (: System.IO.StreamWriter (-> string StreamWriter))))
```

- The alias comes **first and unwrapped**: `(Alias (: Clr.Name sig))`, not
  `(: Alias (: Clr.Name sig))`.
- `StringBuilder` is bound to `System.IO.StreamWriter`; it should be
  `System.Text.StringBuilder` — and a `StringBuilder` is not a port. For string
  output the .NET type is `System.IO.StringWriter`.
- `System.Io.File.Open` → `System.IO.File.Open`, and it needs a signature.
- `import/class` is missing its closing paren, so `import/extern` is nested
  inside it.
- `BinaryReader` has no constructor signature, so it cannot be constructed.

Also note that `StreamReader`/`StreamWriter` are the *concrete* classes;
importing `System.IO.TextReader` and `System.IO.TextWriter` as the port types —
and constructing them via `StreamReader`/`StringReader` — is what makes file and
string ports the same type.
