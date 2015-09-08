import           Distribution.Simple
import           System.Process (system)

main = defaultMainWithHooks
         simpleUserHooks { preConf = \args confFlags -> do
           system "./build-parser.sh"
           preConf simpleUserHooks args confFlags }

