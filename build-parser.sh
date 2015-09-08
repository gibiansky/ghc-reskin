#!/bin/bash -e
# Called from Setup.hs.

# Compile the parser and remove intermediate file.
happy Parser.y

# Move output Haskell to source directory.
mkdir -p src/Reskin
mv Parser.hs src/Reskin/Parser.hs
