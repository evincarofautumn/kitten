module Report
  ( reportAll
  ) where

import Kitten.Report (Report)
import System.IO (hPutStrLn, stderr)
import qualified Kitten.Report as Report
import qualified Text.PrettyPrint as Pretty

reportAll :: [Report] -> IO ()
reportAll = mapM_ $ hPutStrLn stderr . Pretty.render . Report.human
