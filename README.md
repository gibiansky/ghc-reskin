# Reskin

`ghc-reskin` is a tool based on `ghc-exactprint` which allows quick
experimentation with the Haskell language syntax. It provides a single
executable, `ghc-reskin`, which accepts the following arguments:

- `<source file>`: The name of the source file it is processing.
- `<input file>`: Where to read the source file from.
- `<output file>`: Where to write its output to.
- Extensions: After the first three positional arguments, `ghc-reskin` accepts extension flags which change the syntax of the source language. 

`ghc-reskin` is meant to be used as a preprocessor with GHC via the `-F -pgmF` flags. To do, provide arguments to GHC, as in the following example:

```
$ ghc Test.hs -F -pgmF ghc-reskin -optF -XArgumentBlock
```

This can also be used in a pragma, as in the examples below.

# Reskin Installation

To install `ghc-reskin`, clone the repository:

```bash
git clone https://github.com/gibiansky/ghc-reskin.git
```

Enter the repository directory and run `stack install`:

```bash
cd ghc-reskin
stack install
```

If you encounter issues with the parser being generated (or get errors about
`Parser.hs` not being found), run `stack clean` and then `cabal configure`

# Reskin Extensions

Currently supported extensions include:

- `-XArgumentBlock`: This extension allows lambdas, `case`, and `do` arguments to functions to be provided without a `$`. For example, the following function is valid:

```haskell
{-# OPTIONS_GHC -F -pgmF ghc-reskin -optF -XArgumentBlock #-}
module Main where

import System.Environment (getArgs)
import Control.Monad

main :: IO ()
main = do
  args <- getArgs

  -- No $ before the lambda
  long <- forM args \arg ->
    return (length arg > 10)

  -- No $ before `case`
  let test = not case long of
               [] -> True
               _ -> False

  -- No $ before `do`
  when test do
    putStrLn "Test succeeded!"
```
