#!/bin/bash
set -e

echo "Building standard library..."

# In dependency order: `maths` imports nothing, and `prelude` imports it in
# order to re-export the `Num` trait. Building `prelude` first would find a
# stale `maths.dll`, or none at all and fall back to compiling the source a
# second time into `prelude` itself.
dotnet run --lib lib/std/maths.bjo
dotnet run --lib lib/std/prelude.bjo

echo "Standard library built successfully!"
