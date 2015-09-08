module Main where

import           System.Environment (getArgs)
import           System.IO (hPutStrLn, stderr)

import           Reskin (parseReskinOptions, ReskinOptions(..), reskin)

main :: IO ()
main = do
  args <- getArgs
  case args of
    originalFilename:inputFile:outputFile:opts ->
      case parseReskinOptions opts of
        Left err -> hPutStrLn stderr $
          unlines ["Could not parse ghc-reskin options:", unwords opts, err]
        Right reskinOpts ->
          reskin
            reskinOpts
              { reskinSourceFilename = Just originalFilename
              , reskinInputFilename = Just inputFile
              , reskinOutputFilename = Just outputFile
              }
    _ ->
      hPutStrLn stderr $
        unlines ["Usage: ./ghc-reskin <source-filename> <in-file> <out-file> <reskin-opts>"]
