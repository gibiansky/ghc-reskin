module Reskin.Annotate (addReskinAnnotation) where

import           Lexer (P(..), comment_q)
import           SrcLoc (noLoc, Located)
import           ApiAnnotation (AnnotationComment(..))

import           Reskin.Types (ReskinExtension)

addReskinAnnotation :: ReskinExtension -> P (Located a) -> P (Located a)
addReskinAnnotation ext (P parserCont) = 
  P $ \pstate -> parserCont pstate { comment_q = newAnnotation : comment_q pstate}
  where
    newAnnotation = noLoc (AnnLineComment (show ext))
