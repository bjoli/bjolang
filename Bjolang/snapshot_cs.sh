#!/bin/bash
# Snapshot the generated C# for every test file into a directory, so that
# refactors can be diffed for output equivalence.
#
#   ./snapshot_cs.sh baseline     # before a change
#   ./snapshot_cs.sh after        # after a change
#   diff -r cs-snapshots/baseline cs-snapshots/after
#
# 06_modules_and_input.bjo reads stdin, so stdin is redirected from /dev/null
# throughout.

set -u
outdir="cs-snapshots/${1:?usage: snapshot_cs.sh <name>}"
rm -rf "$outdir"
mkdir -p "$outdir"

status=0
for bjo in TestFiles/[0-9][0-9]_*.bjo TestFiles/probe_*.bjo; do
    [ -e "$bjo" ] || continue
    base=$(basename "$bjo" .bjo)
    rm -f out.cs
    dotnet run --no-build "$bjo" --debug </dev/null >"$outdir/$base.log" 2>&1
    # out.cs is written by the frontend before the C# backend is invoked, so it
    # exists whenever the frontend succeeded — which is what we want to compare.
    if [ -f out.cs ]; then
        cp out.cs "$outdir/$base.cs"
    else
        # Frontend failed: the diagnostic is itself the observable behaviour.
        echo "FRONTEND-FAILED" > "$outdir/$base.cs"
        grep -E 'Panicked' "$outdir/$base.log" | head -3 >> "$outdir/$base.cs"
    fi
done

echo "Snapshot written to $outdir"
exit 0
