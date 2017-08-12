module Report
  ( reportAll
  ) where

import Kitten.Report (Report)
import System.IO (hPutStrLn, stderr)
import qualified Kitten.Report as Report
import qualified Text.PrettyPrint as Pretty

reportAll :: [Report] -> IO ()
reportAll = mapM_ $ hPutStrLn stderr . Pretty.renderStyle style . Report.human
  where
    style = Pretty.Style
      { Pretty.mode = Pretty.LeftMode
      , Pretty.lineLength = 80
      , Pretty.ribbonsPerLine = 1.5
      }
