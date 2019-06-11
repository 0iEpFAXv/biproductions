module Main where

import ClassyPrelude
import BiProductionsLib
import System.Environment
import System.Random
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text

absPath :: FilePath -> FilePath
absPath f = f -- "/home/" ++ f

main :: IO ()
main = do
    mLinearizer <- readLinearizer (absPath "samples.txt") (absPath "possLists.txt")
    mWordGenerator <- readWordGenerator (absPath "corpus.txt")
    writeOutput mLinearizer mWordGenerator    

oneSentence :: Linearizer Text -> WordGenerator Text -> IO (InputSentence, Sentence Int)
oneSentence linearizer wordGenerator = generateSentence linearizer wordGenerator 5 <$> newStdGen

readDefault :: Read a => a -> Text -> a
readDefault d t = fromMaybe d $ readMay t

readDefaultSetting :: Read a => a -> Text -> [[Maybe (Text,[Text])]] -> a
readDefaultSetting d var settings = maybe d (readDefault d) $ Map.lookup var s
    where s = Map.fromList $ mapMaybe toAssociation settings
          toAssociation [Just (key,_),Just (value,_)] = Just (key, value)
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
        preprocess = map (map ( uncons . drop 1 . Text.split (== '"') ) . Text.split (== ':')) .
                     Text.split (== ',') . 
                     filter (/= '}') . 
                     filter (/= '{') . pack
    return $ readA result

encodeWordExamples :: OneHotCaps -> WordOrdinals Int -> (InputSentence, Sentence Int) -> [(Either Text [Int], Either Text Int)]
encodeWordExamples caps ordinals (is, ls) = map encodePair wIds
    where wIds = [0..((length is)-1)]
          encodePair wId = (encodeInputWords wId ordinals caps is, encodeOutputWord wId caps ls)

encodeIsAnalogyExamples :: OneHotCaps -> WordOrdinals Int -> (InputSentence, Sentence Int) -> [(Either Text [Int], Either Text Int)]
encodeIsAnalogyExamples caps ordinals (is, ls) = map encodePair wIds
    where wIds = [0..((length is)-1)]
          encodePair wId = (encodeInputWords wId ordinals caps is, encodeOutputIsAnalogy wId caps ls)

encodeTenseExamples :: OneHotCaps -> WordOrdinals Int -> (InputSentence, Sentence Int) -> (Either Text [Int], Either Text Integer)
encodeTenseExamples caps ordinals (is, ls) = encodePair
    where encodePair = (encodeInputWords pivotId ordinals caps is, encodeTenses caps ls 0)
          pivotId = min (1 + ohcSplit caps) ((length is)-1)

writeOutput :: Maybe (Linearizer Text) -> Maybe ([Text], WordGenerator Text) -> IO ()
writeOutput (Just linearizer) (Just (dictionary, wordGenerator)) = do
    count <- getDefaultEnv 1000 "count"
    sentences <- replicateM count $ oneSentence linearizer wordGenerator :: IO [(InputSentence, Sentence Int)]
    let ordinals = wordOrdinals dictionary
        wordExamples = concatMap (encodeWordExamples defaultOneHotCaps ordinals) sentences
        isAnalogyExamples = concatMap (encodeIsAnalogyExamples defaultOneHotCaps ordinals) sentences
        tenseExamples = map (encodeTenseExamples defaultOneHotCaps ordinals) sentences
        showExample (Right iWords, Right oWord) = Text.intercalate "," (map tshow iWords) ++ "," ++ tshow oWord ++ "\n"
        showExample (Left iErr, Right oWord) = iErr ++ "," ++ tshow oWord ++ "\n"
        showExample (Right iWords, Left oErr) = Text.intercalate "," (map tshow iWords) ++ "," ++ oErr ++ "\n"
        showExample (Left iErr, Left oErr) = iErr ++ "," ++ oErr ++ "\n"
        showTextExamples examples = concatMap showExample examples
    writeFileUtf8 (absPath "sentences.txt") $ tshow sentences
    writeFileUtf8 (absPath "isAnalogyTraining.txt") $ showTextExamples isAnalogyExamples
    writeFileUtf8 (absPath "wordTraining.txt") $ showTextExamples wordExamples
    writeFileUtf8 (absPath "tensesTraining.txt") $ showTextExamples tenseExamples
    
writeOutput _ _ = writeFileExamples "/home/samples.txt" "/home/wordLists.txt"
