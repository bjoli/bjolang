# Modules, imports and includes

Every `.bjo` file is one module. Its module name is the file name with `.` and
`-` replaced by `_`, and it compiles to a C# static class named
`<module>_Module`.

There are three ways to pull in code from elsewhere, and they do different
things.

| Form | Creates a module? | Makes names visible | Links an assembly |
|---|---|---|---|
| `(import (std core))` | yes, the imported one | its exports and re-exports | yes |
| `(include "helper.bjo")` | no — splices into *this* module | everything in the file | n/a |
| transitive `BjolangDeps` | no | nothing | yes |

## `export`

```scheme
(export print println)
```

Names a module's public surface. Every exported name **must have an explicit
signature in the same module**:

```scheme
(: println (-> %a void))
(defun (println x) ...)
(export println)
```

This is deliberate. An exported type is part of a compiled DLL's metadata, so it
has to be written down rather than inferred.

## `re-export`

```scheme
(re-export print println)
```

Publishes names this module imported from somewhere else. The local-signature
rule does not apply, because a re-exported name already carries a signature from
where it was defined; what is checked instead is that the name is actually in
scope here.

This is how an aggregate module works:

```scheme
;; std/prelude.bjo
(import (std core) (std list) (std iter))

(re-export print println)
```

Anything importing `(std prelude)` sees `print` and `println` — and nothing else
from `core`, `list` or `iter`.

## Linking is not importing

A compiled library records its dependencies in `BjolangDeps`. Those assemblies
are **referenced** when you build against the library, because that is where the
code of anything re-exported through it actually lives — but their exports are
not imported. Only what a module explicitly exports or re-exports is visible
through it.

Note the asymmetry that follows: at the C# level a `using static` is emitted for
every linked assembly, so all of their static classes are technically in scope.
Visibility is enforced by the Bjolang type checker, not by C#.

> Historical note: this used to work the other way round. Importing a DLL pulled
> in the exports of everything it transitively depended on, which is why
> `(import (std prelude))` gave you `print` and `println` even though
> `prelude.bjo` re-exported nothing at all and `prelude.dll` carried no export
> metadata. The names were arriving from `core` directly; `prelude` was a
> dependency bundle rather than a module with a surface.

## `include`

```scheme
(include "inc/math_helpers.bjo")
```

Splices the named file's top-level forms in at that position, as if they had
been typed there. Paths resolve relative to the directory of the file doing the
including, so a chain of includes follows the files.

An include creates **no module**. That has two consequences:

- The included definitions are in scope immediately. They do not need to be
  exported, and they are not visible to anyone importing you unless you export
  them yourself.
- Everything ends up in one generated class.

Includes nest, and an included file's own `(import ...)` forms become imports of
the including module.

Cycles, missing files and malformed forms are reported with a file and a line:

```
Include Error: 'a.bjo' includes itself at b.bjo:1. Include chain: a.bjo -> b.bjo -> a.bjo
Include Error: cannot find 'nope.bjo' included at m.bjo:1 (looked for /.../nope.bjo)
Include Error: malformed include at m.bjo:1. Expected (include "path")
```

### Prefer `include` over a source-level `import`

`(import "helper.bjo")` — importing a `.bjo` for which no `.dll` exists — is
currently **broken**. The file is type-checked and a `using static` is emitted
for it, but the code generator only emits the last module, so the generated C#
references a class that was never produced:

```
error CS0246: The type or namespace name 'helper_Module' could not be found
```

See `TestFiles/srcimport/` for a reproduction. Until that is fixed, use
`include` to split a module across files, or compile the other file to a DLL
first with `--lib` and import that.

## Building the standard library

`lib/std/*.bjo` must be compiled to DLLs before anything can import them:

```
./build_std.sh
```

`resolveImportPath` prefers a `.dll` over a `.bjo` of the same name, so this is
also what keeps `(import (std ...))` off the broken source-import path.

`BjolangDeps` stores **absolute** paths, and a dependency that no longer exists
at its recorded path is silently skipped. Moving the checkout therefore breaks
re-exported names with a confusing "Unbound variable" rather than a missing-file
error. Re-run `build_std.sh` after relocating the tree.
