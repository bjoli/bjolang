#!/bin/bash
# Snapshot generated C# for the sources that actually contain trait `impl`s.
# The numbered TestFiles corpus contains none, so `generateDecl`'s TImpl arm is
# otherwise unverified.
set -u
outdir="/tmp/impl_${1:?usage: snapshot_impl.sh <name>}"
rm -rf "$outdir"; mkdir -p "$outdir"

cp -r lib/std /tmp/std_backup_$$

for src in lib/std/core.bjo lib/std/iter.bjo TestFiles/traitparsetest.bjo; do
    base=$(basename "$src" .bjo)
    rm -f out.cs
    dotnet run --no-build --lib "$src" </dev/null >"$outdir/$base.log" 2>&1
    if [ -f out.cs ]; then cp out.cs "$outdir/$base.cs"; else echo FAILED > "$outdir/$base.cs"; fi
done

rm -rf lib/std; mv /tmp/std_backup_$$ lib/std
echo "impl snapshot -> $outdir"
