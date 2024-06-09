#!/bin/sh
# difftool-dune.sh

echo "Arguments received: $@"
file="$2"
if [ -z "$file" ]; then
    file="$1"
fi
echo "File to diff: $file"

# Check if file exists in the specified location or in the current directory
if [ ! -f "$file" ]; then
    if [ -f "$(basename "$file")" ]; then
        file="$(basename "$file")"
    else
        echo "File does not exist: $file"
        exit 1
    fi
fi

echo "Running: dune exec -- diffcessible $file"
dune exec -- diffcessible "$file"

