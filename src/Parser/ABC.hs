{-# LANGUAGE DeriveGeneric, BlockArguments, OverloadedStrings #-}

module Parser.ABC (
  ABCTree(..),
  CCGcat(..),
  abcTree2sentence,
  abcTree2verbList,
  abc2verbListFromDirectory,
  parseABCtext,
  parseABCfromFile,
  parseABCfromDirectory,
  isErr,
  isVerb,
  checkVerbList,
  abc2sentenceFromDirectory,
  writeABCsentenceFromFile
  -- writeMaskedABCsentenceFromFile
  ) where

import GHC.Generics --base
import System.FilePath --filepath
import qualified Data.Text    as T --text
import qualified Data.Text.IO as T --text
import qualified Data.Aeson            as A --aeson
import qualified Data.ByteString.Char8 as B --bytestring 
import qualified Data.Yaml             as Y --yaml
import qualified Debug.Trace as D
import qualified Text.Show.Unicode as U
import Text.Parsec
import Text.Parsec.Text --parsec
import Data.List
import Text.Directory (checkFile,checkDir,getFileList) --nlp-tools
import System.Directory(createDirectoryIfMissing)


--データ構造を定義
data ABCTree =
    Word T.Text
    -- Phrase NP [("role,"a")] [ABCTree]
    | Phrase CCGcat CCGft [ABCTree]
    -- | ID ABCTree T.Text
    | ID T.Text
    | Err String T.Text
    deriving (Eq, Show, Read, Generic)

-- CCGカテゴリー
data CCGcat = 
  CP | CPf | CPt |CPt_sbj | CPq | CPq_sbj | CPx | FRAG | N | Ns | 
  NP | NPq| NPR | NUM | NUMCLP | PP | PPs | PPs2 | PPo1 | PPo2 |
  PRO | Q | S | Sa | Se | Simp | Sm | Snml | Srel | Ssmc | Ssub | 
  WNUM | WPRO | INTJP | INTJ | LST | LS | PRN | MULTI_SENTENCE | SYM
  | Slash CCGcat CCGcat
  | BSlash CCGcat CCGcat
  | ERR
  deriving (Eq, Show, Read)

-- ex: [("role","a"),("deriv","unary-IPADV")
type CCGft = [(String, String)]

-- instance A.FromJSON ABCTree
-- instance A.ToJSON ABCTree  


-- ABCTreebankの各文について、
checkVerbList :: ABCTree -> Bool
checkVerbList abc = do
  -- abcTree2verbListを適用し各開始位置、終了位置を求める
  let verblist = abcTree2verbList abc in
    -- abcTree2sentenceを適用する
    let sentence = abcTree2sentence abc in
      -- verblistの情報からabcTree2sentenceの部分文字列を切り出し、verblistと一致しているかを調べる
      and $ map (checkVerbList' sentence) verblist
  

checkVerbList' :: T.Text -> (T.Text,CCGcat,Int,Int) -> Bool
checkVerbList' sentence (verb, cat, start, end) = case verb of
  "" -> 
    if (start > end) then True
    else False
  _ -> 
    if (start > end) then False
    -- verbの先頭文字がsentenceのstart位置文字と一致
    else if ((T.unpack sentence) !! start) == (head (T.unpack verb)) then 
    -- verbの先頭をdropしたもので再帰
      checkVerbList' sentence (T.pack (drop 1 (T.unpack verb)), cat, start+1, end) 
    else False
  
-- String型のカテゴリーをCCGcat型に変換する
str2CCGcat :: String -> CCGcat
str2CCGcat s = 
  case s of
    "CP" -> CP; "CPf" -> CPf; "CPt" -> CPt; "CPt-sbj" -> CPt_sbj; "CPq" -> CPq; "CPq-sbj" -> CPq_sbj; "CPx" -> CPx
    "FRAG" -> FRAG
    "N" -> N ; "Ns" -> Ns; "NP" -> NP; "NPq" -> NPq
    "NPR" -> NPR
    "NUM" -> NUM
    "NUMCLP" -> NUMCLP
    "PP" -> PP; "PPs" -> PPs; "PPs2" -> PPs2; "PPo1" -> PPo1; "PPo2" -> PPo2
    "PRO" -> PRO
    "Q" -> Q
    "S" -> S; "Sa"-> Sa; "Se" -> Se; "Simp"->Simp; "Sm" -> Sm; "Snml"->Snml; "Srel" -> Srel; "Ssmc" -> Ssmc; "Ssub" -> Ssub
    "WNUM" -> WNUM
    "WPRO" -> WPRO
    "INTJP" -> INTJP; "INTJ" -> INTJ 
    "LST" -> LST; "LS" -> LS; "PRN" -> PRN; "multi-sentence" -> MULTI_SENTENCE
    _ -> ERR
    
abcTree2sentence :: ABCTree -> T.Text
abcTree2sentence (Word word) = case T.uncons word of
                                 Just ('*',_) -> T.empty
                                 Just ('_',_) -> T.empty -- __lex_predを除外
                                 _ -> word
abcTree2sentence (Phrase _ _ dtrs) = T.concat $ map abcTree2sentence dtrs
abcTree2sentence (ID i) = i
-- abcTree2sentence (ID tree id) = T.concat [abcTree2sentence tree, id] 
abcTree2sentence (Err _ text) = T.concat ["(Err ", text,")"]

abcTree2maskedSentence :: ABCTree -> T.Text
abcTree2maskedSentence (Word word) = case T.uncons word of 
                                       Just ('*',_) -> T.empty
                                       _ -> word
abcTree2maskedSentence (Phrase cat _ dtrs) = case cat of
  N -> T.pack "[MASK]"
  _ -> T.concat $ map abcTree2maskedSentence dtrs
abcTree2maskedSentence (ID i) = i
abcTree2maskedSentence (Err _ text) = T.concat ["(Err ", text,")"]


-- abcTree2verbList ABCTree -> (表層系,範疇, 開始位置, 終了位置)
abcTree2verbList :: ABCTree -> [(T.Text,CCGcat,Int,Int)]
abcTree2verbList tree = 
  let lst = quadrupleList tree in
    abcTree2verbList' lst
    where 
      -- abcTree2verbList' :: [(T.Text,CCGcat,Int,Int)] -> [(T.Text,CCGcat,Int,Int)]
      abcTree2verbList' list = 
        case list of
          [] -> []
          (pf,cat,start,end):xs -> 
            if isVerb cat then (pf,cat,start,end) : abcTree2verbList' xs
            else abcTree2verbList' xs

-- [("人",N,0,0),("が",BSlash NP PPs,1,1),("集まる",BSlash PPs Sm,2,4),("。",BSlash Sm Sm,5,5)]
-- (表層系,範疇,開始位置,終了位置)
quadrupleList :: ABCTree-> [(T.Text,CCGcat,Int,Int)]
quadrupleList tree = 
  let wordlst = wordList tree in
    let catlst = catList tree in 
      let endlst = drop 1 $ map (subtract 1) (scanl (+) 0 (map length wordlst)) in
        let startlst = zipWith (\word end -> end - (length word) + 1) wordlst endlst in
          let lst = zip4 (map T.pack wordlst) catlst startlst endlst in lst
            -- D.trace (U.ushow lst) lst 

-- ABCTreeのwordを繋げた文字列
-- ["人","が","集まる","。"]
wordList :: ABCTree -> [String]
wordList (Word word) =
  case T.uncons word of
    Just ('*',_) -> []
    Just ('_',_) -> []
    _ -> [T.unpack word]
  --  [T.unpack word]                 
wordList (Phrase _ _ abc) = concat (map wordList abc)
wordList (ID _) = []
wordList (Err _ _) = ["ERR"]

-- abcTreeのCCGcatのみのリストを返す
-- [N,BSlash NP PPo1,BSlash PPo1 (BSlash PPs Sm)]
catList :: ABCTree -> [CCGcat]
catList (Word word) = []
catList (Phrase cat ft abc) = case abc of
  [] -> []
  [Word word] -> case T.uncons word of
    Just ('*',_) -> []
    Just ('_',_) -> []
    _ -> [cat]
  lists -> concat (map catList lists)


-- Sと S+featureのみTrue
isSentence :: CCGcat -> Bool
isSentence cat = 
  case cat of
   S->True; Sa->True;Se->True;Simp->True;Sm->True
   Snml->True; Srel->True;Ssmc->True;Ssub->True
   _ -> False
 
isErr :: ABCTree -> Bool
isErr abc = case abc of
  Word _ -> False
  Phrase _ _ _ -> False
  ID _ -> False
  Err _ _  -> True

-- 動詞だけのリストを取り出す
--動詞かどうかを判定する
-- 形容詞をはじく?
isVerb' :: CCGcat -> Bool
isVerb' cat =
  if isSentence cat then True
  else case cat of
    Slash s x -> 
      -- Slash S Sは除外
      if(isSentence s && isSentence x) then False
      -- xが用言なら除外
      else if (isVerb' x) then False
      -- sがSかどうかを再起で調べる
      else isVerb' s
    BSlash x s -> 
      -- BSlash S Sは除外
      if(isSentence x && isSentence s) then False
      -- xが用言なら除外
      else if (isVerb' x) then False
      -- sがSかどうかを再起で調べる
      else isVerb' s
    _ -> False

-- -- ただのSはFalseにする
isVerb :: CCGcat -> Bool
isVerb cat = 
  if(isSentence cat) then False
  else isVerb' cat 

-- | ABC (parsed)のためのパーザ
parseABCtext :: T.Text -> [ABCTree]
parseABCtext psd =
  case parse abcTreesParser "" psd of
    Left err -> [Err (show err) psd]
    Right trees -> trees

-- | 空白または改行
blank :: Parser String
blank = many1 $ oneOf " \t\n"

-- | 空白や括弧でない文字列
str :: Parser String
str = many1 $ noneOf " \t\n()#=<>/\\"

str' :: Parser String
str' = many1 $ noneOf " \t\n()#=<>\\"

abcTreesParser :: Parser [ABCTree]
abcTreesParser = do
  trees <- sepBy1' abcTreeParser blank
  return trees

-- | PTBの構文木は必ず一番外側に余計な()がつく。
abcTreeParser :: Parser ABCTree
abcTreeParser = do
  _ <- char '('
  _ <- optional blank
  tree <- (phraseParser <|> wordParser)
  _ <- optional blank
  _ <- idParser
  _ <- char ')' <|> (blank >> char ')')
  -- return $ ID tree (T.pack id)
  return tree

-- <PPs\Ssub>みたいなやつをパーズ
-- CCGcatParser
catParser :: Parser CCGcat
catParser = do
    _ <- char '<'
    cat1 <- catParser
    slash <- oneOf "\\/"
    cat2 <- catParser
    _ <- char '>'
    case slash of
      '/' -> return $ Slash cat1 cat2
      '\\' -> return $ BSlash cat1 cat2
  <|>
  do 
    cat <- str
    return $ str2CCGcat cat

-- ID部分をパーズ
idParser :: Parser ABCTree
idParser = do
    _ <- char '('
    _ <- string "ID"
    _ <- char ' '
    title <- str'
    _ <- char ')'
    return $ ID (T.pack title)

-- 語をパーズ
wordParser :: Parser ABCTree
wordParser = do    
    word <- str
    return $ Word (T.pack word)

-- 副次的な素性のパーズ
featureParser :: Parser (String,String)
featureParser = do
    _ <- char '#'
    rule <- str
    _ <- char '='
    feature <- str
    return $ (rule,feature)    
    
featuresParser :: Parser CCGft
featuresParser = do
  features <- many featureParser
  return features

-- | フレーズのパーズ
phraseParser :: Parser ABCTree
phraseParser = do
  _ <- char '('
  -- syncatはCCGcat型
  syncat <- catParser
  sharps <- featuresParser
  _ <- blank
  abcs <- sepBy1' (phraseParser <|> wordParser) blank
  _ <- char ')' <|> (blank >> char ')')
  return $ Phrase syncat sharps abcs

sepBy1' :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a]
{-# INLINABLE sepBy1' #-}
sepBy1' p sep = do 
  x <- p
  xs <- many $ try (sep >> p)
  return $ x:xs

parseABCfromFile :: FilePath -> IO [ABCTree]
parseABCfromFile abcFilePath = do
  checkFile abcFilePath
  abc <- T.readFile abcFilePath
  return $ parseABCtext abc

-- abcファイルと同じ名前のtxtファイルを作成し、abcのsentenceのみを書き出す
writeABCsentenceFromFile :: FilePath -> IO()
writeABCsentenceFromFile abcFilePath = do
  --   -- abc::[ABCTree]
  abc <- parseABCfromFile abcFilePath
  let dirName = takeDirectory abcFilePath
  let baseName = takeBaseName abcFilePath 
  -- directoryがなければ作る
  -- createDirectoryIfMissing True (dirName </> "ABCsentences")
   -- sentences :: [T.Text]
  let sentences = map abcTree2sentence abc in 
    -- .txtファイルのファイルパスを作成
    let textFilePath = addExtension (dirName </> baseName) "txt" in
    -- let textFilePath = addExtension (dirName </> "ABCsentences" </> baseName) "txt" in 
      -- writeFile :: FilePath -> String -> IO ()
      writeFile textFilePath (unlines $ map T.unpack sentences)


-- abcファイルと同じ名前のtxtファイルを作成し、abcのsentenceのみを書き出す
writeABCverbListFromFile :: FilePath -> IO()
writeABCverbListFromFile abcFilePath = do
  --   -- abc::[ABCTree]
  abc <- parseABCfromFile abcFilePath
  let dirName = takeDirectory abcFilePath
  let baseName = takeBaseName abcFilePath 
  -- directoryがなければ作る
  createDirectoryIfMissing True (dirName </> "ABCverbs")
   -- sentences :: [T.Text]
   -- abcTree2verbList :: abctree -> [(T.Text,CCGcat,Int,Int)]
   -- verblist :: [[(T.Text,CCGcat,Int,Int)]]
  let verblist = concat $ map abcTree2verbList abc
  let verbs = nub $ map (\(v,cat,s,e) -> v) verblist in 
  -- let sentences = map abcTree2sentence abc in 
    -- .txtファイルのファイルパスを作成
    let textFilePath = addExtension (dirName </> "ABCverbs" </> baseName) "txt" in
      -- writeFile :: FilePath -> String -> IO ()
      writeFile textFilePath (unlines $ map T.unpack verbs)


parseABCfromDirectory :: FilePath -> String -> IO [ABCTree]
parseABCfromDirectory abcDirectoryPath ext = do
  -- abcDirectoryPathが存在するディレクトリであることを確認する。
  checkDir abcDirectoryPath
  -- abcDirectoryPath以下のディレクトリ（再帰的）にある拡張子extのファイルのリストを得る
  filePaths <- getFileList ext abcDirectoryPath
  -- filePathの中のreadme以外のファイルでparseABCfromFileを行う
  abcss <- mapM parseABCfromFile $ filter (\f -> takeBaseName f /= "readme") filePaths
  return $ concat abcss

abc2sentenceFromDirectory :: FilePath -> String -> IO()
abc2sentenceFromDirectory abcDirectoryPath ext = do
  -- abcDirectoryPathが存在するディレクトリであることを確認する。
  checkDir abcDirectoryPath
  -- abcDirectoryPath以下のディレクトリ（再帰的）にある拡張子extのファイルのリストを得る
  filePaths <- getFileList ext abcDirectoryPath 
  -- filePathの中のreadme以外のファイルでabc2sentenceFromFileを行う  
  let filePaths' = filter (\f -> takeBaseName f /= "readme") filePaths in
    writeABCsentenceFromFile' filePaths'
    where 
      -- 再帰用関数
      -- writeABCsentenceFromFile' :: [FilePath] -> IO()
      writeABCsentenceFromFile' filepaths = case filepaths of
        [] -> return ()
        file:files ->  D.trace ("parsing: " ++ file) (do
          writeABCsentenceFromFile file
          writeABCsentenceFromFile' files)

abc2verbListFromDirectory :: FilePath -> String -> IO()
abc2verbListFromDirectory abcDirectoryPath ext = do
  -- abcDirectoryPathが存在するディレクトリであることを確認する。
  checkDir abcDirectoryPath
  -- abcDirectoryPath以下のディレクトリ（再帰的）にある拡張子extのファイルのリストを得る
  filePaths <- getFileList ext abcDirectoryPath 
  -- filePathの中のreadme以外のファイルでabc2sentenceFromFileを行う  
  let filePaths' = filter (\f -> takeBaseName f /= "readme") filePaths in
    writeABCverbListFromFile' filePaths'
    where 
      -- 再帰用関数
      -- writeABCsentenceFromFile' :: [FilePath] -> IO()
      writeABCverbListFromFile' filepaths = case filepaths of
        [] -> return ()
        file:files ->  D.trace ("parsing: " ++ file) (do
          writeABCverbListFromFile file
          writeABCverbListFromFile' files)
  
      