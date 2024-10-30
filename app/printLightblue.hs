{-# LANGUAGE OverloadedStrings #-}

import qualified System.IO as S           --base
import Parser.Language.Japanese.MyLexicon(emptyCategories)
import Interface(printNodes,Style(..))

main :: IO()
main = do
    let ec = emptyCategories
    printNodes S.stdout HTML 0 "" True ec