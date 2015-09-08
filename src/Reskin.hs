module Reskin (parseReskinOptions, ReskinOptions(..), reskin) where

import           Data.List (stripPrefix, nub, (\\), partition)
import           Control.Monad
import qualified Data.Map as Map
import           Data.Maybe


-- GHC API imports.
import qualified ApiAnnotation as GHC
import qualified DynFlags as GHC
import qualified FastString as GHC
import qualified StringBuffer as GHC
import qualified GHC hiding (parseModule)
import qualified HeaderInfo as GHC
import qualified Lexer as GHC
import qualified MonadUtils as GHC
import qualified Outputable as GHC
import qualified Parser as GHC
import qualified SrcLoc as GHC
import           GHC.Paths (libdir)

import           Language.Haskell.GHC.ExactPrint (exactPrintWithAnns, relativiseApiAnnsWithComments)
import           Language.Haskell.GHC.ExactPrint.Preprocess (stripLinePragmas)
import           Language.Haskell.GHC.ExactPrint.Types (Anns, annNone, AnnKey(..), AnnConName(..),
                                                        DeltaPos(..), Annotation(..), KeywordId(..))

import           Reskin.Parser (parseModule)
import           Reskin.Types (ReskinOptions(..), ReskinExtension(..))


parseReskinOptions :: [String] -> Either String ReskinOptions
parseReskinOptions args = do
  stripped <- mapM stripDashX args
  exts <- mapM readEither stripped
  return
    ReskinOptions
      { reskinExtensions = exts
      , reskinSourceFilename = Nothing
      , reskinInputFilename = Nothing
      , reskinOutputFilename = Nothing
      }

  where
    readEither :: Read a => String -> Either String a
    readEither str =
      case readMay str of
        Nothing -> Left $ "Could not parse input " ++ str
        Just a  -> Right a

    stripDashX :: String -> Either String String
    stripDashX str =
      case stripPrefix "-X" str of
        Nothing -> Left $ str ++ " does not start with -X"
        Just x  -> Right x

readMay :: Read a => String -> Maybe a
readMay str =
  case reads str of
    (a, ""):_ -> Just a
    _         -> Nothing

reskin :: ReskinOptions -> IO ()
reskin reskinOpts =
  case reskinOpts of
    ReskinOptions
      { reskinSourceFilename = Just src
      , reskinInputFilename = Just inp
      , reskinOutputFilename = Just out
      , reskinExtensions = exts
      } -> do
      result <- parseFile src inp
      case result of
        Left (_, str) -> fail str
        Right (usedExts, anns, pmod) ->
          -- Check if we're using an extensions we're not allowed to.
          case usedExts \\ exts of
            []   -> writeFile out $ exactPrintWithAnns pmod (fixAnns anns)
            exts -> fail $ unwords ["Using reskin extension that were not enabled:", show exts]

    _ -> fail "Source filename and/or input filename not provided to reskin."
  where

    fixAnns = Map.insert (AnnKey GHC.noSrcSpan (CN "HsPar"))
                (annNone { annsDP = [(G GHC.AnnOpenP, DP (0, 1)), (G GHC.AnnCloseP, DP (0, 0))] })

extractExtensions :: [GHC.Located GHC.AnnotationComment] -> ([ReskinExtension], [GHC.Located GHC.AnnotationComment])
extractExtensions commentAnns = (extensions, otherAnns)
  where
    (extAnns, otherAnns) = partition (isExtensionComment . GHC.unLoc) commentAnns
    extensions = nub $ mapMaybe (readAnnotation . GHC.unLoc) extAnns

    readAnnotation :: Read a => GHC.AnnotationComment -> Maybe a
    readAnnotation (GHC.AnnLineComment str) = readMay str
    readAnnotation _ = Nothing

    isExtensionComment :: GHC.AnnotationComment -> Bool
    isExtensionComment = isJust . fmap ext . readAnnotation

    ext :: ReskinExtension -> ReskinExtension
    ext = id

-- | Run the reskin module parser on a string.
parseFile :: FilePath  -- ^ The original name of the source file (for error messages).
          -> FilePath   -- ^ The file from which to read the module.
          -> IO (Either (GHC.SrcSpan, String) ([ReskinExtension], Anns, GHC.Located (GHC.HsModule GHC.RdrName)))
parseFile sourceFile file =
  GHC.defaultErrorHandler GHC.defaultFatalMessager GHC.defaultFlushOut $
    GHC.runGhc (Just libdir) $ do
      dflags <- initDynFlags file
      txt <- GHC.liftIO $ readFile file
      let (fileContents, injectedComments) = stripLinePragmas txt
      return $
        case runParser parseModule dflags sourceFile fileContents of
          GHC.PFailed ss m -> Left (ss, GHC.showSDoc dflags m)
          GHC.POk pstate pmod ->
            let (exts, anns) =  extractExtensions (GHC.comment_q pstate)
            in Right
                 (exts, relativiseApiAnnsWithComments injectedComments pmod (mkApiAnns pstate anns), pmod)

-- | From @Language.Haskell.GHC.ExactPrint.Parsers@
initDynFlags :: GHC.GhcMonad m => FilePath -> m GHC.DynFlags
initDynFlags file = do
  dflags0 <- GHC.getSessionDynFlags
  let dflags1 = GHC.gopt_set dflags0 GHC.Opt_KeepRawTokenStream
  src_opts <- GHC.liftIO $ GHC.getOptionsFromFile dflags1 file
  (dflags2, _, _) <- GHC.parseDynamicFilePragma dflags1 src_opts
  void $ GHC.setSessionDynFlags dflags2
  return dflags2

-- | From @Language.Haskell.GHC.ExactPrint.Parsers@
runParser :: GHC.P a -> GHC.DynFlags -> FilePath -> String -> GHC.ParseResult a
runParser parser flags filename str = GHC.unP parser parseState
    where
      location = GHC.mkRealSrcLoc (GHC.mkFastString filename) 1 1
      buffer = GHC.stringToStringBuffer str
      parseState = GHC.mkPState flags buffer location

-- | From @Language.Haskell.GHC.ExactPrint.Parsers@
mkApiAnns pstate comms
  = ( Map.fromListWith (++) . GHC.annotations $ pstate
    , Map.fromList ((GHC.noSrcSpan, comms) : GHC.annotations_comments pstate))
