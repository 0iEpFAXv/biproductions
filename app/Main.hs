module Main where

import ClassyPrelude
import BiProductionsLib
import System.Environment
import System.Random
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text

absPath :: FilePath -> FilePath
absPath f = "/home/" ++ f

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
        readA (Right t) = readDefaultSetting d var $ traceShowId $ preprocess (traceShowId t)
        preprocess = map (map ( uncons . drop 1 . Text.split (== '"') ) . Text.split (== ':')) .
                     Text.split (== ',') . 
                     filter (/= '}') . 
                     filter (/= '{') . pack
    return $ readA result

writeOutput :: Maybe (Linearizer Text) -> Maybe ([Text], WordGenerator Text) -> IO ()
writeOutput (Just linearizer) (Just (dictionary, wordGenerator)) = do
    count <- getDefaultEnv 1000 "count"
    sentences <- replicateM count $ oneSentence linearizer wordGenerator
    let ordinals = buildWordOrdinals dictionary
        encodeInput = encodeOneHotInput ordinals defaultOneHotCaps
        encodeOutput = encodeOneHotOutput defaultOneHotCaps
        encodedSentences :: [(InputSentence, Sentence Int)] -> [(Either Text [Int], Either Text [Int])]
        encodedSentences = map (\(i,o) -> (encodeInput i, encodeOutput o))
        showEncodedSentence (Right iBits, Right oBits) = "(" ++ tshow iBits ++ "," ++ tshow oBits ++ ")"
        showEncodedSentence (Left iErr, Right oBits) = "(" ++ iErr ++ "," ++ tshow oBits ++ ")"
        showEncodedSentence (Right iBits, Left oErr) = "(" ++ tshow iBits ++ "," ++ oErr ++ ")"
        showEncodedSentence (Left iErr, Left oErr) = "(" ++ iErr ++ "," ++ oErr ++ ")"
        textSentences = map showEncodedSentence $ encodedSentences sentences
    writeFileUtf8 (absPath "sentences.txt") $ tshow sentences
    writeFileUtf8 (absPath "encodedSentences.txt") $ tshow textSentences
    
writeOutput _ _ = writeFileExamples "/home/samples.txt" "/home/wordLists.txt"
