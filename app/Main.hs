module Main where

import ClassyPrelude hiding (intercalate)
import BiProductionsLib
import System.Environment
import System.Random hiding (split)
import qualified Data.Map.Strict as Map
import Data.Text (split, intercalate)
import qualified Data.Text.Lazy as LT
import qualified Data.ByteString as BS

absPath :: FilePath -> FilePath -> FilePath
absPath path f = path ++ f

main :: IO ()
main = do
    stage <- getDefaultEnv "A" "stage"
    writeOutput stage

readDefault :: Read a => a -> Text -> a
readDefault d t = fromMaybe d $ readMay t

readDefaultSetting :: Read a => a -> Text -> [[Text]] -> a
readDefaultSetting d var settings = maybe d (readDefault d) $ Map.lookup var s
    where s = Map.fromList $ mapMaybe toAssociation settings
          toAssociation [key,value] = Just (key, value)
          toAssociation _ = Nothing

-- "ANALYTIC_SETTINGS" will look something like: 
-- "{ \"path\":\"\"\"\",
--    \"count\": \"3\",
--    \"stage\": \"B\" }"
getDefaultEnv :: (Read a, Show a) => a -> Text -> IO a
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
    return $ traceShow (readA result) (readA result)

encodeModifierExamples :: OneHotCaps -> WordOrdinals Int -> (InputSentence, Sentence Int) -> [(Either Text [Int], Either Text Int)]
encodeModifierExamples caps ordinals (is, ls) = map encodePair wIds
    where wIds = [0..(length is-1)]
          encodePair wId = (encodeSentenceInputWords ls wId ordinals caps is, encodeOutputModifier wId caps ls)

encodeIsExamples :: OneHotCaps -> WordOrdinals Int -> Analogy Bool -> (InputSentence, Sentence Int) -> [(Either Text [Int], Either Text Int)]
encodeIsExamples caps ordinals which (is, ls) = map encodePair wIds
    where wIds = [0..(length is-1)]
          encodePair wId = (encodeInputWords wId ordinals caps is, encodeOutputIs which wId caps ls)

encodeSOVIsExamples :: OneHotCaps -> WordOrdinals Int -> Analogy Bool -> (InputSentence, Sentence Int) -> [(Either Text [Int], Either Text Int)]
encodeSOVIsExamples caps ordinals which (is, ls) = filter (\ePair -> Right 0 /= snd ePair) $ map encodePair wIds
    where wIds = [0..(length is-1)]
          encodePair wId = (encodeSentenceInputWords ls wId ordinals caps is, encodeOutputIs which wId caps ls)

encodeTenseExamples :: OneHotCaps -> Int -> Bool -> WordOrdinals Int -> (InputSentence, Sentence Int) -> (Either Text [Int], Either Text Integer)
encodeTenseExamples caps tId isStart ordinals (is, ls) = encodePair
    where encodePair = (encodeSentenceInputWords ls pivotId ordinals caps is, encodeTense ls tId isStart)
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

writeOutput :: Text -> IO ()
writeOutput "A" = do
    path <- getDefaultEnv "/home/data/" "path"
    --putStrLn $ "Found path: " ++ path
    writeFileExamples (absPath path "samples.txt") (absPath path "wordLists.txt") 
writeOutput "B" = do
    path <- getDefaultEnv "/home/data/" "path"
    (Just linearizer) <- readLinearizer (absPath path "samples.txt") (absPath path "possLists.txt")
    (Just (dictionary, wordGenerator)) <- readWordGenerator (absPath path "corpus.txt")
    count <- getDefaultEnv 1000 "count"
    stdGen <- newStdGen
    let ordinals = wordOrdinals dictionary
        modifierExample = encodeModifierExamples defaultOneHotCaps ordinals
        isVerbExample = encodeIsExamples defaultOneHotCaps ordinals (Analogy 0 False False True)
        isSubjectExample = encodeIsExamples defaultOneHotCaps ordinals (Analogy 0 True False False)
        isObjectExample = encodeIsExamples defaultOneHotCaps ordinals (Analogy 0 False True False)
        subjectOfVerbExample = encodeSOVIsExamples defaultOneHotCaps ordinals (Analogy 0 True False True)
        objectOfVerbExample = encodeSOVIsExamples defaultOneHotCaps ordinals (Analogy 0 False True True)
        tenseExample tId isStart = encodeTenseExamples defaultOneHotCaps tId isStart ordinals
        showExample :: (Show a) => (Either Text [Int], Either Text a) -> Text
        showExample (Right iWords, Right oWord) = intercalate "," (map tshow iWords) ++ "," ++ tshow oWord ++ "\n"
        showExample (Left iErr, Right oWord) = iErr ++ "," ++ tshow oWord ++ "\n"
        showExample (Right iWords, Left oErr) = intercalate "," (map tshow iWords) ++ "," ++ oErr ++ "\n"
        showExample (Left iErr, Left oErr) = iErr ++ "," ++ oErr ++ "\n"
        sentences = showSentences linearizer wordGenerator stdGen count
    writeFileUtf8 (absPath path "sentences.txt") "["
    lazyAppendFileUtf8 (absPath path "sentences.txt") $ LT.intercalate ",\n" (map LT.fromStrict (sentences tshow))
    appendFileUtf8s (absPath path "sentences.txt") ["]"]
    writeFileUtf8s (absPath path "isVerbTraining.txt") $ sentences (concatMap showExample . isVerbExample)
    writeFileUtf8s (absPath path "isSubjectTraining.txt") $ sentences (concatMap showExample . isSubjectExample)
    writeFileUtf8s (absPath path "isObjectTraining.txt") $ sentences (concatMap showExample . isObjectExample)
    writeFileUtf8s (absPath path "objectOfVerbTraining.txt") $ sentences (concatMap showExample . objectOfVerbExample)
    writeFileUtf8s (absPath path "subjectOfVerbTraining.txt") $ sentences (concatMap showExample . subjectOfVerbExample)
    writeFileUtf8s (absPath path "modifierTraining.txt") $ sentences (concatMap showExample . modifierExample)
    writeFileUtf8s (absPath path "tensesTraining1S.txt") $ sentences (showExample . tenseExample 0 True)
    writeFileUtf8s (absPath path "tensesTraining1E.txt") $ sentences (showExample . tenseExample 0 False)
    writeFileUtf8s (absPath path "tensesTraining2S.txt") $ sentences (showExample . tenseExample 1 True)
    writeFileUtf8s (absPath path "tensesTraining2E.txt") $ sentences (showExample . tenseExample 1 False)
writeOutput "C" = do
    path <- getDefaultEnv "/home/data/" "path"
    (Just (dictionary, _)) <- readWordGenerator (absPath path "corpus.txt")
    (Just texts) <- maybeReadUtf8 (absPath path "texts.txt")
    let ordinals = wordOrdinals dictionary
        inputSentences :: [InputSentence]
        inputSentences = map (fromList . map InputText) texts
        wIds is = [0..(length is-1)]
        inputWords is wId = encodeInputWords wId ordinals defaultOneHotCaps is
        convertSentence is = map (inputWords is) (wIds is)
        sentences = concatMap convertSentence inputSentences
        showSentence :: Either Text [Int] -> Text
        showSentence (Right iWords) = intercalate "," (map tshow iWords) ++ "\n"
        showSentence (Left iErr) = iErr ++ "\n"
    writeFileUtf8s (absPath path "encodedInputs.txt") $ map showSentence sentences
writeOutput "D" = do
    path <- getDefaultEnv "/home/data/" "path"
    (Just (dictionary, _)) <- readWordGenerator (absPath path "corpus.txt")
    (Just texts) <- maybeReadUtf8 (absPath path "texts.txt")
    (Just isVerbs) <- maybeReadUtf8 (absPath path "isVerbs.txt")
    (Just isSubjects) <- maybeReadUtf8 (absPath path "isSubjects.txt")
    (Just isObjects) <- maybeReadUtf8 (absPath path "isObjects.txt")
    let ordinals = wordOrdinals dictionary
        inputSentences :: [InputSentence]
        inputSentences = map (fromList . map InputText) texts
        r1 :: [Float] -> Int 
        r1 l = decodeBitList $ map (\x -> if x > m-0.01 then 1 else 0) l
            where m = maximum $ ncons 0 l
        r2 :: [[Float]] -> [Int]
        r2 l = map r1 l
        r3 :: [[[Float]]] -> [[Int]]
        r3 l = map r2 l
        infoSentences = zip4 (r3 isSubjects) (r3 isObjects) (r3 isVerbs) inputSentences
        wIds (_,_,_,is) = [0..(length is-1)]
        inputWords (subjects,objects,verbs,is) wId = encodeSOVInputWords subjects objects verbs wId ordinals defaultOneHotCaps is
        sentences = concatMap (\is -> map (inputWords is) (wIds is)) infoSentences
        showSentence :: Either Text [Int] -> Text
        showSentence (Right iWords) = intercalate "," (map tshow iWords) ++ "\n"
        showSentence (Left iErr) = iErr ++ "\n"
    writeFileUtf8s (absPath path "encodedSOVInputs.txt") $ map showSentence sentences
writeOutput "E" = do
    path <- getDefaultEnv "/home/data/" "path"
    (Just ohTense1S) <- maybeReadUtf8 (absPath path "ohTense1S.txt")
    (Just ohTense1E) <- maybeReadUtf8 (absPath path "ohTense1E.txt")
    (Just ohTense2S) <- maybeReadUtf8 (absPath path "ohTense2S.txt")
    (Just ohTense2E) <- maybeReadUtf8 (absPath path "ohTense2E.txt")
    (Just ohMs) <- maybeReadUtf8 (absPath path "ohModifiers.txt")
    (Just ohSoVs) <- maybeReadUtf8 (absPath path "ohSubjectOfVerbs.txt")
    (Just ohOoVs) <- maybeReadUtf8 (absPath path "ohObjectOfVerbs.txt")
    (Just texts) <- maybeReadUtf8 (absPath path "texts.txt")
    let r1 :: [Float] -> [Int] 
        r1 l = map (\x -> if x > m-0.01 then 1 else 0) l
            where m = maximum $ ncons 0 l
        r2 :: [[Float]] -> [[Int]]
        r2 l = map r1 l
        r3 :: [[[Float]]] -> [[[Int]]]
        r3 l = map r2 l
        ohTenses :: [[[Int]]]
        ohTenses = map (\(s, e, ss, ee) -> [s,e,ss,ee]) $ zip4 (r2 ohTense1S) (r2 ohTense1E) (r2 ohTense2S) (r2 ohTense2E)
        infoBlocks :: [([[Int]], [[Int]], [[Int]], [[Int]])]
        infoBlocks = zip4 ohTenses (r3 ohMs) (r3 ohSoVs) (r3 ohOoVs)
        sentenceCodings = map (\(ts, ms, soVs, ooVs) -> decodeSentence defaultOneHotCaps ts ms soVs ooVs) infoBlocks
        idToText :: [Text] -> Int -> Text
        idToText s wId = Map.findWithDefault "<missing>" wId $ Map.fromList (zip [0..1] s)
        insertWords (_, Left err) = Left err
        insertWords (m, Right s) = Right $ fmap m s
        sentences = map insertWords $ zip (map idToText texts) sentenceCodings
    writeFileUtf8 (absPath path "sentenceCodings.txt") "["
    lazyAppendFileUtf8 (absPath path "sentenceCodings.txt") $ LT.intercalate ",\n" (map LT.fromStrict (map tshow sentenceCodings))
    appendFileUtf8s (absPath path "sentenceCodings.txt") ["]"]
    writeFileUtf8 (absPath path "sentences.txt") "["
    lazyAppendFileUtf8 (absPath path "sentences.txt") $ LT.intercalate ",\n" (map LT.fromStrict (map tshow sentences))
    appendFileUtf8s (absPath path "sentences.txt") ["]"]

writeOutput stage = putStrLn $ "Error: Stage " ++ stage ++ " is not defined!"
