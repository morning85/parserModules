module Parser.JumanKatuyou (
    JumanKatuyouGata(..),
    JumanKatuyouKei(..),
    parseKatuyouFromPath
 ) where

import qualified Data.Text as T    --text
import qualified Data.Text.IO as T --text
import           Text.Directory (checkFile) --nlp-tools
import           Text.Parsec
import           Text.Parsec.Text --parsec


-- juman.katuyou
data JumanKatuyouGata = 
    KatuyouGata (T.Text,[JumanKatuyouKei])
    | Err String
    deriving (Eq, Show, Read)

data JumanKatuyouKei = 
    KatuyouKei (T.Text, T.Text)
    | Comment T.Text
    deriving (Eq, Show, Read)


-- ファイルをパーズする
parseKatuyouFromPath :: FilePath -> IO [JumanKatuyouGata]
parseKatuyouFromPath filepath = do
    -- let filepath = "JUMAN.katuyou"
    checkFile filepath
    katuyou <- T.readFile filepath
    return $ parseJumanKatuyouGata katuyou

-- | パーザ
parseJumanKatuyouGata :: T.Text -> [JumanKatuyouGata]
parseJumanKatuyouGata txt =
    case parse jumanKatuyouGatasParser "" txt of
        Left err -> [Err (show err)]
        Right katuyou -> katuyou


-- | 活用型用パーザ
jumanKatuyouGatasParser :: Parser [JumanKatuyouGata]
jumanKatuyouGatasParser = do
  verbKatuyous <- many jumanKatuyouGataParser
  _ <- many blank
  adjKatuyous <- many jumanKatuyouGataParser
  _ <- many blank
  hanteishiKatuyous <- many jumanKatuyouGataParser
  return $ verbKatuyous ++ adjKatuyous ++ hanteishiKatuyous

-- | 活用型用パーザ
jumanKatuyouGataParser :: Parser JumanKatuyouGata
jumanKatuyouGataParser = do
    _ <- optional $ many $ jumanCommentParser
    -- _ <- char '\n'
    _ <- char '('
    katuyougata <- katuyouGataParser
    _ <- char ')'
    _ <- string "\n\n"
    return katuyougata
    where 
        jumanCommentParser :: Parser JumanKatuyouKei
        jumanCommentParser = do
            _ <- many1 $ char ';'
            _ <- optional (many $ char ' ')
            _ <- optional $ str'
            _ <- char '\n'
            _ <- optional blank
            return $ Comment ""
            where 
                str' = many1 $ noneOf "\t\n#="

-- | 活用型内のパーザー
katuyouGataParser :: Parser JumanKatuyouGata
katuyouGataParser = do
    katuyougata' <- str' 
    _ <- optional blank
    _ <- optional $ many commentParser
    (katuyougata,katuyoukei) <- (kahenGataParser <|> (katuyouGataParser'))
    let gata = katuyougata' ++ katuyougata
    return $ KatuyouGata (T.pack gata, katuyoukei)
    where 
        str' :: Parser String 
        str' = many1 $ noneOf " \t\n()#=<>/\\来;"
        commentParser :: Parser String
        commentParser = do
            -- _ <- string "\n\n"
            _ <- string "; "
            _ <- optional $ string "※ " -- 判定詞用
            _ <- str'
            _ <- char '\n'
            -- _ <- str'
            _ <- optional blank
            return $ ""
-- カ変以外
katuyouGataParser' :: Parser (String, [JumanKatuyouKei])
katuyouGataParser' = do
    -- _ <- blank
    _ <- char '('
    katuyoukeis <- many1 (katuyouKeiParser <|> izenOnbinParser)
    _ <- optional blank
    _ <- char ')' 
    _ <- blank 
    return $ ("", katuyoukeis)

--　カ変
kahenGataParser ::  Parser (String, [JumanKatuyouKei])
kahenGataParser = do
    _ <- string "来"
    _ <- blank
    _ <- char '('
    katuyoukeis <- many1 (kahenKatuyouKeiParser <|> kahenIzenParser)
    _ <- optional blank
    _ <- char ')'
    _ <- blank
    return $ ("来", katuyoukeis)

-- | 活用形用パーザ
katuyouKeiParser :: Parser JumanKatuyouKei
katuyouKeiParser = do
    _ <- char '('
    katuyoukei <- str
    _ <- blank
    katuyougobi <- str
    _ <- char ')' <|> (blank >> char ')')
    _ <- optional $ (many $ char ' ')
    _ <- optional jumanCommentParser
    _ <- blank
    return $ KatuyouKei (T.pack katuyoukei, T.pack katuyougobi)
    where 
        jumanCommentParser :: Parser JumanKatuyouKei
        jumanCommentParser = do
            let str' = many1 $ noneOf "\t\n()#="
            _ <- char ';'
            _ <- optional (many $ char ' ')
            comment <- str'
            return $ Comment (T.pack comment)                

-- | カ変活用形用パーザ
kahenKatuyouKeiParser :: Parser JumanKatuyouKei
kahenKatuyouKeiParser = do
    _ <- char '('
    katuyoukei <- str
    _ <- blank
    gobi1 <- str
    _  <- blank
    gobi2 <- str
    _ <- char ')'
    -- コメントはない
    _ <- blank
    return $ KatuyouKei (T.pack katuyoukei, T.pack (gobi1 ++ "/" ++ gobi2))


-- カ変已然形用
kahenIzenParser :: Parser JumanKatuyouKei
kahenIzenParser = do
    _ <- char ';'
    _ <- blank
    _ <- char '('
    katuyoukei <- str
    _ <- blank
    gobi1 <- str
    _  <- blank
    gobi2 <- str
    _ <- char ')'
    -- コメントはない
    _ <- blank
    return $ KatuyouKei (T.pack katuyoukei, T.pack (gobi1 ++ "/" ++ gobi2))

-- 已然形か、音便条件系がない場合
izenOnbinParser :: Parser JumanKatuyouKei
izenOnbinParser = do
    _ <- char ';'
    _ <- blank
    katuyoukei <- izenkeiParser <|> onbinkeiParser
    return katuyoukei

izenkeiParser :: Parser JumanKatuyouKei
izenkeiParser = do
    _ <- char '('
    katuyoukei <- str
    _ <- blank
    katuyougobi <- str
    _ <- blank
    _ <- string ")\n"
    _ <- optional blank
    return $ KatuyouKei (T.pack katuyoukei, T.pack katuyougobi)

-- 音便条件形がない場合
onbinkeiParser :: Parser JumanKatuyouKei
onbinkeiParser = do
    _ <- string "音便条件形 はない"
    _ <- blank
    return $ KatuyouKei (T.pack "音便条件形", T.pack "")

-- | 空白や括弧でない文字列
str :: Parser String
str = many1 $ noneOf " \t\n()#=<>\\;"

-- | 空白または改行
blank :: Parser String
blank = many1 $ oneOf " \t\n"

