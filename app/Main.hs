module Main where

import ClassyPrelude hiding (intercalate)
import BiProductionsLib
import System.Environment
import System.Random hiding (split)
import qualified Data.Map.Strict as Map
--import qualified Data.Text as Text
import Data.Text (split, intercalate)
import qualified Data.Text.Lazy as LT
import qualified Data.ByteString as BS

absPath :: FilePath -> FilePath -> FilePath
absPath path f = path ++ f

main :: IO ()
main = do
    path <- getDefaultEnv "/home/" "path"
    mLinearizer <- readLinearizer (absPath path "samples.txt") (absPath path "possLists.txt")
    mWordGenerator <- readWordGenerator (absPath path "corpus.txt")
    writeOutput mLinearizer mWordGenerator    

readDefault :: Read a => a -> Text -> a
readDefault d t = fromMaybe d $ readMay t

readDefaultSetting :: Read a => a -> Text -> [[Text]] -> a
readDefaultSetting d var settings = maybe d (readDefault d) $ Map.lookup var s
    where s = Map.fromList $ mapMaybe toAssociation settings
          toAssociation [key,value] = Just (key, value)
          toAssociation _ = Nothing

-- "ANALYTIC_SETTINGS" will look something like: 
-- "{ \"function\":\"fft\",
--    \"n\": \"0\",
--    \"axis\": \"x\",
--    \"norm\": \"max\" }"
getDefaultEnv :: Read a => a -> Text -> IO a
getDefaultEnv d var = do
    result <- tryIO $ getEnv "ANALYTIC_SETTINGS"
    let readA (Left _) = d
        readA (Right t) = readDefaultSetting d var $ preprocess t
        preprocess = map (map (intercalate "\"" . 
                               (\ts -> take (length ts - 1) ts) . 
                               drop 1 . 
                               split (== '"') ) . split (== ':')) .
                     split (== ',') . 
                     filter (/= '}') . 
                     filter (/= '{') . pack
    return $ readA result

encodeWordExamples :: OneHotCaps -> WordOrdinals Int -> (InputSentence, Sentence Int) -> [(Either Text [Int], Either Text Int)]
encodeWordExamples caps ordinals (is, ls) = map encodePair wIds
    where wIds = [0..(length is-1)]
          encodePair wId = (encodeInputWords wId ordinals caps is, encodeOutputWord wId caps ls)

encodeIsAnalogyExamples :: OneHotCaps -> WordOrdinals Int -> (InputSentence, Sentence Int) -> [(Either Text [Int], Either Text Int)]
encodeIsAnalogyExamples caps ordinals (is, ls) = map encodePair wIds
    where wIds = [0..(length is-1)]
          encodePair wId = (encodeInputWords wId ordinals caps is, encodeOutputIsAnalogy wId caps ls)

encodeTenseExamples :: OneHotCaps -> WordOrdinals Int -> (InputSentence, Sentence Int) -> (Either Text [Int], Either Text Integer)
encodeTenseExamples caps ordinals (is, ls) = encodePair
    where encodePair = (encodeInputWords pivotId ordinals caps is, encodeTenses caps ls 0)
          pivotId = min (1 + ohcSplit caps) (length is-1)

showSentences :: Linearizer Text -> WordGenerator Text -> StdGen -> Int -> ((InputSentence, Sentence Int) -> Text) -> [Text]
showSentences _ _ _ 0 _ = []
showSentences linearizer wordGenerator g count showFn = showFn sentence : showSentences linearizer wordGenerator g' (count-1) showFn
    where (sentence, g') = generateSentence linearizer wordGenerator 5 g

writeFileUtf8s :: (MonadIO m) => FilePath -> [Text] -> m ()
writeFileUtf8s fp [] = liftIO . BS.writeFile fp . encodeUtf8 $ ""
writeFileUtf8s fp (c:chunks) = do
    liftIO . BS.writeFile fp . encodeUtf8 $ c
    appendFileUtf8s fp chunks

lazyWriteFileUtf8 :: (MonadIO m) => FilePath -> LT.Text -> m ()
lazyWriteFileUtf8 fp = writeFileUtf8s fp . toChunks

appendFileUtf8s :: (MonadIO m) => FilePath -> [Text] -> m ()
appendFileUtf8s fp = mapM_ (liftIO . BS.appendFile fp . encodeUtf8)

lazyAppendFileUtf8 :: (MonadIO m) => FilePath -> LT.Text -> m ()
lazyAppendFileUtf8 fp = appendFileUtf8s fp . toChunks

writeOutput :: Maybe (Linearizer Text) -> Maybe ([Text], WordGenerator Text) -> IO ()
writeOutput (Just linearizer) (Just (dictionary, wordGenerator)) = do
    count <- getDefaultEnv 1000 "count"
    stdGen <- newStdGen
    path <- getDefaultEnv "/home/" "path"
    let ordinals = wordOrdinals dictionary
        wordExample = encodeWordExamples defaultOneHotCaps ordinals
        isAnalogyExample = encodeIsAnalogyExamples defaultOneHotCaps ordinals
        tenseExample = encodeTenseExamples defaultOneHotCaps ordinals
        showExample :: (Show a) => (Either Text [Int], Either Text a) -> Text
        showExample (Right iWords, Right oWord) = intercalate "," (map tshow iWords) ++ "," ++ tshow oWord ++ "\n"
        showExample (Left iErr, Right oWord) = iErr ++ "," ++ tshow oWord ++ "\n"
        showExample (Right iWords, Left oErr) = intercalate "," (map tshow iWords) ++ "," ++ oErr ++ "\n"
        showExample (Left iErr, Left oErr) = iErr ++ "," ++ oErr ++ "\n"
        sentences = showSentences linearizer wordGenerator stdGen count
    writeFileUtf8 (absPath path "sentences.txt") $ "["
    lazyAppendFileUtf8 (absPath path "sentences.txt") $ LT.intercalate ",\n" (map LT.fromStrict (sentences tshow))
    appendFileUtf8s (absPath path "sentences.txt") $ ["]"]
    writeFileUtf8s (absPath path "isAnalogyTraining.txt") $ sentences (concatMap showExample . isAnalogyExample)
    writeFileUtf8s (absPath path "wordTraining.txt") $ sentences (concatMap showExample . wordExample)
    writeFileUtf8s (absPath path "tensesTraining.txt") $ sentences (showExample . tenseExample)
    
writeOutput _ _ = writeFileExamples "/home/samples.txt" "/home/wordLists.txt"
