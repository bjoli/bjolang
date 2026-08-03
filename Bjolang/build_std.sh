#!/bin/bash
set -e

echo "Building standard library..."
dotnet run --lib lib/std/prelude.bjo

echo "Standard library built successfully!"
