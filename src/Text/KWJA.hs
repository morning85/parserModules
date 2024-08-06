-- {-# HADDOCK Markdown #-}
{- |
Module      : Text.KWJA
Description : This is documentation tests.
Copyright   : (c) morning85, 2024
License     : All right reserved
Maintainer  : Asa Tomita <tomita.asa@is.ocha.ac.jp> 
Stability   : experimental

A parser for the output text of a Japanese text analyzer, KWJA.
-}

module Text.KWJA (
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
import qualified Debug.Trace as D       -- for debug
import Text.Parsec                      -- parsec
import Text.Parsec.Text                 -- parsec
import Text.Parsec.Combinator           -- parsec
import Data.Char (isDigit)              -- base
import Text.Directory (checkFile,checkDir,getFileList) --nlp-tools


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
    -- it can be D | P | A | I     
    nodeType :: Char,   
    -- |list of argument structures  
    args :: Maybe [Arg],    
    -- | category
    -- it can be 体言 | 用言 | [用言,体言]
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


-- | The 'square' function squares an integer.
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
    char '+'
    sep
    dest <- (try (string "-1")) <|> (many1 digit)
    nodeType <- oneOf "DPAI"
    optional sep
    optional $ try ne                               -- NEの情報は捨てる
    args <-  optionMaybe $ many argParser           -- 項構造データ(ないときもある
    cats <- optionMaybe $ many (try kwjaCategory)   -- 体言 | 用言 | [用言,体言]
    optional $ try sm                               -- <SM-主体>は捨てる
    tense <- optionMaybe $ try tenseLevelParser         -- 時制
    level <- optionMaybe $ try tenseLevelParser         -- レベル
    others <- optionMaybe $ many (othersParser)     -- その他
    return $ KWJA KWJANode {dest = (read dest),
                            nodeType = nodeType,
                            args = args,
                            cats = cats,
                            tense = tense,
                            level = level,
                            others = others}

-- <rel type="ガ" target="太郎" sid="202407121752-0-1" id="0"/>
argParser :: Parser Arg
argParser = do
    try $ string "<rel type="
    argType <- quoteParser
    sep
    string "target="
    target <- quoteParser
    optional sep
    sid <- optionMaybe $ idParser "sid"
    argId <- optionMaybe $ idParser "id"
    string "/>"
    return $ Arg {argType = argType, 
                    target = target, 
                    sid = sid, 
                    argId = argId}
    where 
        idParser :: String -> Parser Int
        idParser str = do
            string $ str ++ "="
            id <- quoteParser
            optional sep
            return $ read $ (filter (\x -> isDigit x) id)

-- | 体言か用言かをパーズする
kwjaCategory :: Parser T.Text
kwjaCategory = do
    try (char '<')
    s <- (string "用") <|> (string "体")
    cat <- many $ noneOf ">"
    char '>'
    return $ T.pack $ s ++ cat

-- | 時制とレベルをパーズする
tenseLevelParser :: Parser T.Text
tenseLevelParser = do
    try (char '<')
    s <- (string "時制") <|> (string "レベル")
    char ':'
    tenseOrLevel <- many $ noneOf ">"
    char '>'
    return $ T.pack tenseOrLevel

-- | レベル以降の情報をパーズする
othersParser :: Parser T.Text
othersParser = do
    try (char '<')
    s <- many $ noneOf ">"
    char '>'
    return $ T.pack s

-- | Named Entityとしての情報
ne :: Parser ()
ne = do
  try $ string "<NE:"
  many1 $ noneOf ">"
  char '>'
  return ()

-- | <SM-主体>
sm :: Parser ()
sm = do
  try $ string "<SM-"
  many1 $ noneOf ">"
  char '>'
  return ()
