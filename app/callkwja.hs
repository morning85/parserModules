{-# LANGUAGE OverloadedStrings #-}

import qualified System.Environment as E --base
import qualified Text.KWJA as KW              
import qualified Data.Text as T               --text
import qualified Data.Text.IO as T            --text
import qualified Text.Show.Unicode as U

main :: IO()
main = do
  (text:_) <- E.getArgs


---kwja ------------------------------------------
  kwjas <-  KW.callKWJA (T.pack text)
  U.uprint text
  U.uprint kwjas