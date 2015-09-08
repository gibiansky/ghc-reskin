module Reskin.Types (ReskinOptions(..), ReskinExtension(..)) where

data ReskinOptions =
       ReskinOptions
         { reskinExtensions :: [ReskinExtension]
         , reskinSourceFilename :: Maybe FilePath
         , reskinInputFilename :: Maybe FilePath
         , reskinOutputFilename :: Maybe FilePath
         }
  deriving (Eq, Show)

data ReskinExtension = ArgumentBlock
  deriving (Eq, Read, Show)
