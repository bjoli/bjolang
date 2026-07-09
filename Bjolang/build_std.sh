#!/bin/bash
set -e

echo "Building standard library..."
dotnet run --lib lib/std/list.bjo
dotnet run --lib lib/std/iter.bjo
dotnet run --lib lib/std/core.bjo
dotnet run --lib lib/std/prelude.bjo

echo "Standard library built successfully!"
