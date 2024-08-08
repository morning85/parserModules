{-# LANGUAGE OverloadedStrings #-}
-- {-# HADDOCK Markdown #-}
{- |
Module      : Parser.KWJA
Copyright   : (c) morning85, 2024
License     : All right reserved
Maintainer  : Asa Tomita <tomita.asa@is.ocha.ac.jp> 
Stability   : experimental

A parser for the output text of a Japanese text analyzer, KWJA.
-}

module Parser.KWJA (
    KWJAData(..),
    Arg(..),
    KWJANode(..),
    callKWJA,
    parseKWJAFromFile 
    ) where

import qualified Text.Juman as J        -- nlp-tools
import qualified Data.Text as T         -- text
import qualified Data.Text.IO as T      -- text
import qualified Shelly as S            -- shelly
import qualified Text.Show.Unicode as U -- unicode-show
-- import qualified Debug.Trace as D       -- for debug
import Text.Parsec                      -- parsec
import Text.Parsec.Text                 -- parsec
-- import Text.Parsec.Combinator           -- parsec
import Data.Char (isDigit)              -- base
import Text.Directory (checkFile) --nlp-tools


data Arg = Arg {
    -- | case-frame of the argument
    argType :: String,
    -- | target of the argument
    target :: String,
    -- | sidereal time       
    sid :: Maybe Int,      
    -- | destination argument id
    argId :: Maybe Int
} deriving (Eq)


data KWJANode = KWJANode {
    -- | destination node id
    dest :: Int,       
    -- |
    --  D or P or A or I     
    nodeType :: Char,   
    -- |list of argument structures  
    args :: Maybe [Arg],    
    -- | category
    -- 体言 or 用言 or [用言,体言]
    cats :: Maybe [T.Text], 
    -- | tense
    tense :: Maybe T.Text,  
    -- | level
    level :: Maybe T.Text,  
    -- | other KWJA data
    others :: Maybe [T.Text] 
} deriving (Eq)


instance Show Arg where
    show (Arg {argType = a, target = t, sid = s, argId = ai}) = 
        concat ["(",a, ",", t, ",", show s, ",", show ai,")"] 

instance Show KWJANode where
    show (KWJANode {dest=d, nodeType=n, args=a, cats=c, tense=t,level=l,others=o}) = 
        concat ["(",show d, ",", show n, ",", show a, ",", show c, ",", show t, ",", show l, ",", show o, ")"] 

-- | Data format for KWJA analysis line
data KWJAData = 
    -- | line start with +
    KWJA KWJANode
    -- | other lines
    | Juman J.JumanData 
    -- | error
    | Err String T.Text 
    deriving (Eq, Show)


-- | テキストをkwjaで解析し、`KWJAData`として返す
callKWJA :: T.Text -> IO [KWJAData]
callKWJA text = do
    kwjaText <- fromText text
    return $ map kwjaParser kwjaText

-- | ファイル内のテキストに対してKWJAを呼び出し、分析行のリストを返す。
parseKWJAFromFile :: FilePath -> IO([[KWJAData]])
parseKWJAFromFile file = do
    checkFile file
    kwja <- T.readFile file
    let kwjas = T.lines kwja
    mapM callKWJA kwjas

-- | テキストに対しKWJAを呼び出し、分析行のリストを返す。
fromText :: T.Text -> IO([T.Text])
fromText text = do
  kwjaOutput <- S.shelly $ S.silently $ S.escaping False $ S.cmd $ S.fromText $ T.concat ["kwja --text ", text]
  return $ kwjaFilter $ T.lines kwjaOutput

-- | 空白行、#または*から始まる行は除く。
kwjaFilter :: [T.Text] -> [T.Text]
kwjaFilter = filter (\l -> l /= T.empty
                                    && not (T.isPrefixOf "#" l)
                                    && not (T.isPrefixOf "*" l)
                                    ) 

-- |　KWJAの分析行をパーズし、KWJAData形式に変換する。
kwjaParser :: T.Text -> KWJAData
kwjaParser text =
  case parse kwjaLine "" text of
    Left e -> Err (U.ushow e) text
    Right t -> t

kwjaLine :: Parser KWJAData
kwjaLine = kwjaNode <|> jumanNode

jumanNode :: Parser KWJAData
jumanNode = Juman <$> J.jumanLine

-- | 半角空白パーザ。
sep :: Parser ()
sep = do
  _ <- char ' '
  return ()

-- | ""で囲まれた文字列の中身を取り出す
quoteParser :: Parser String
quoteParser = do
    _ <- char '"'
    str <- many $ noneOf ['"']
    _ <- char '"'
    return str

-- | +から始まるkwjaの行をパーズする
kwjaNode :: Parser KWJAData
kwjaNode = do
    _ <- char '+'
    sep
    d <- (try (string "-1")) <|> (many1 digit)
    nt <- oneOf "DPAI"
    optional sep
    optional $ try ne                               -- NEの情報は捨てる
    arg <-  optionMaybe $ many argParser           -- 項構造データ(ないときもある
    c <- optionMaybe $ many (try kwjaCategory)   -- 体言 | 用言 | [用言,体言]
    optional $ try sm                               -- <SM-主体>は捨てる
    t <- optionMaybe $ try tenseLevelParser         -- 時制
    l <- optionMaybe $ try tenseLevelParser         -- レベル
    other <- optionMaybe $ many (othersParser)     -- その他
    return $ KWJA KWJANode {dest = (read d),
                            nodeType = nt,
                            args = arg,
                            cats = c,
                            tense = t,
                            level = l,
                            others = other}

-- <rel type="ガ" target="太郎" sid="202407121752-0-1" id="0"/>
argParser :: Parser Arg
argParser = do
    _ <- try $ string "<rel type="
    at <- quoteParser
    sep
    _ <- string "target="
    tar <- quoteParser
    optional sep
    s <- optionMaybe $ idParser "sid"
    ai <- optionMaybe $ idParser "id"
    _ <- string "/>"
    return $ Arg {argType = at, 
                    target = tar, 
                    sid = s, 
                    argId = ai}
    where 
        idParser :: String -> Parser Int
        idParser str = do
            _ <- string $ str ++ "="
            i <- quoteParser
            optional sep
            return $ read $ (filter (\x -> isDigit x) i)

-- | 体言か用言かをパーズする
kwjaCategory :: Parser T.Text
kwjaCategory = do
    _ <- try (char '<')
    s <- (string "用") <|> (string "体")
    cat <- many $ noneOf ">"
    _ <- char '>'
    return $ T.pack $ s ++ cat

-- | 時制とレベルをパーズする
tenseLevelParser :: Parser T.Text
tenseLevelParser = do
    _ <- try (char '<')
    _ <- (string "時制") <|> (string "レベル")
    _ <- char ':'
    tenseOrLevel <- many $ noneOf ">"
    _ <- char '>'
    return $ T.pack tenseOrLevel

-- | レベル以降の情報をパーズする
othersParser :: Parser T.Text
othersParser = do
    _ <- try (char '<')
    s <- many $ noneOf ">"
    _ <- char '>'
    return $ T.pack s

-- | Named Entityとしての情報
ne :: Parser ()
ne = do
  _ <- try $ string "<NE:"
  _ <- many1 $ noneOf ">"
  _ <- char '>'
  return ()

-- | <SM-主体>
sm :: Parser ()
sm = do
  _ <- try $ string "<SM-"
  _ <- many1 $ noneOf ">"
  _ <- char '>'
  return ()
