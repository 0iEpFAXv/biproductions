module EnglishExamples
    ( BuilderContext, nextM,
      AbstractTime (NTMinusInfinity, NTMinus, NTMinusDelta, NTPast, NTNow, NTFuture, NTPlusDelta, NTPlus, NTPlusInfinity), 
      Tense, Tenses,
      LinearPhrase, WordGenerator,
      encodeBitList, decodeBitList, decodeBitVector, padOrdinal,
      showPossibleWords, showExamples, readExamples, writeFileExamples, readFileExamples, readWordGenerator,
      buildWordGenerator
      ) where

import ClassyPrelude
import qualified Data.Map.Strict as Map
import qualified Data.Vector as Vector
import Control.Monad.Random.Lazy hiding (replicateM)
import Control.Monad.Trans.State.Lazy

data AbstractTime = NTMinusInfinity | NTMinus | NTMinusDelta 
                  | NTPast | NTNow | NTFuture
                  | NTPlusDelta | NTPlus | NTPlusInfinity 
                  deriving (Eq, Read, Show)
type Tense = (AbstractTime, AbstractTime)
type Tenses = Vector Tense

type BuilderContext a = RandT StdGen (State Int) a

type LinearPhrase a = [(Int,a)]
type WordGenerator a = LinearPhrase a -> BuilderContext (LinearPhrase Text)

nextM :: BuilderContext Int
nextM = lift $
    do n <- get
       put $ n+1
       return n

type ModifierId = Int
data ExampleWordLabel = FrontMatter | Modifier ModifierId | MainWord | Other Int | BackMatter deriving (Eq, Show, Read)
data ExampleWord = ExampleWord ExampleWordLabel Text deriving (Eq, Show, Read)
type ExampleWords = [ExampleWord]
data ExampleTableId = SubjectTable Int 
                    | ConjunctionTable 
                    | PrimaryAnalogyTable Tense Int 
                    | AnalogyTable Int
                    | ObjectTable Int deriving (Eq, Show, Read)
data TagSample = TagSample ExampleTableId ExampleWords deriving (Eq, Show, Read)
type TagSamples = [TagSample]

data PossibleWordIndex = OneWord Text | FirstWord Text Text | MidWord Text Text Text | LastWord Text Text deriving (Ord, Eq, Show, Read)
type PossibleWordVector = Vector Text
type PossibleWords = Map PossibleWordIndex PossibleWordVector

padOrdinal :: Int -> [Int] -> [Int]
padOrdinal size l | 2 ^ length l < size = padOrdinal size (0:l)
                  | otherwise = l

encodeBitList :: Int -> [Int]
encodeBitList = reverse . ebv
    where ebv 0 = []
          ebv n = (n `mod` 2) : ebv (n `div` 2)

decodeBitList :: [Int] -> Int
decodeBitList vec = decodeReverseBitList 0 0 $ reverse vec
    where decodeReverseBitList :: Int -> Int -> [Int] -> Int
          decodeReverseBitList _ s [] = s
          decodeReverseBitList power s (v:rest) = 
              decodeReverseBitList (power+1) (s+2^power*v) rest

decodeBitVector :: Vector Int -> Int
decodeBitVector vec = decodeBitList $ Vector.toList vec

convertExampleDataToTagSample :: (ExampleTableId, Maybe ([ExampleWordLabel], [Text])) -> Maybe TagSample
convertExampleDataToTagSample (tableId, (Just (wLs, ws))) = Just $ TagSample tableId (zipWith ExampleWord wLs ws)
convertExampleDatatoTagSample (_, Nothing) = Nothing

--                  info           nounMods  noun   proper  modNum  modNexts
basicExampleData :: ExampleInfo -> [Text] -> Text -> Bool -> Int -> [Bool] -> Maybe ([ExampleWordLabel], [Text])
basicExampleData _ _ [] _ _ _ _ = Nothing
basicExampleData _ _ noun proper _ [] = if proper then Just ([MainWord], [toTitle noun]) else Just ([MainWord], [noun])
basicExampleData _ [] _ _ _ _ (m:_) = Nothing
basicExampleData info (nm:restNounMods) noun proper modNum (modNext:rest) = addCurrent exampleDataRest
    where m = if not modNext || null rest || (modModNotFirst info && [nm] == firstMod info) 
              then nm
              else modMod info
          isProper = properness info && if proper then True else modNum == 0 && modNext != 0
          addCurrent Nothing = Nothing
          addCurrent (Just (wordLabelRest, wordsRest)) = Just (Modifier modNum:wordLabelRest, m:wordsRest)
          exampleDataRest = basicExampleData modMod restNounMods noun False isProper (modNum+1) rest

data ExampleInfo = ExampleInfo { modMod :: Text
                               , extWordLabel :: Int -> [Text]
                               , extWord :: [Text]
                               , firstMod :: [Text]
                               , properness :: Bool
                               , modModNotFirst :: Bool}

-- The first argument is a list of modifier counts to scoop up and build with basicNounExampleData
--                           groups  extras     info         nounMods   nouns    modNum  modNexts   
exampleDataWithDescriptor :: [Int] -> [Int] -> ExampleInfo -> [Text] -> [Text] -> Int -> [Bool] -> Maybe ([ExampleWordLabel], [Text])
exampleDataWithDescriptor [] _ _ _ _ _ _ _ = Nothing
exampleDataWithDescriptor _ [] _ _ _ _ _ _ = Nothing
exampleDataWithDescriptor _ _ _ [] _ _ _ _ = Nothing
exampleDataWithDescriptor _ _ _ _ [] _ _ _ = Nothing
exampleDataWithDescriptor (0:rest) = exampleDataWithDescriptor rest
exampleDataWithDescriptor (n:rest) (ext:restExt) info nounMods (noun:restNouns) modNum modNexts = 
    let groupNext = uncons $ drop (n-1) modNexts
        --If the nth modifier is compatible with splitting into a descriptor, it will be False followed by a non empty list
        canSplit = null groupNext || groupNext == Just (False, []) || groupNext == Just (True, [])
        
        combineResults (Just (basicWLs, basicWs)) (Just (False, _)) (Just (descWLs, descWs)) =
            Just (basicWLs++(extWordLabel info ext)++descWLs', basicWs++(extWord info)++descWs)
                where maxModNum = foldr foldMax (modNum+n) descWLs
                      foldMax (Modifier x) y = if x > y then x else y 
                      descWLs' = map (\MainWord -> Modifier maxModNum) descWLs
        combineResults _ _ _ = Nothing

        basicRs = basicExampleData info (firstMod info ++ take n nounMods) noun False modNum (take n modNexts)
        descriptorRs = exampleDataWithDescriptor rest restExt info (drop n nounMods) restNouns (modNum+n) (drop (n+1) modNexts)
    in if not canSplit
       then basicRs
       else combineResults basicRs groupNext descriptorRs

nounExampleInfo = ExampleInfo { modMod = "very", extWordLabel = (\x -> [Other x]), extWord = ["of"], firstMod = ["the"], properness = True, modModNotFirst = False}
verbExampleInfo = ExampleInfo { modMod = "very", extWordLabel = (\x -> []), extWord = [], firstMod = [], properness = False, modModNotFirst = False }
verbFutureExampleInfo = ExampleInfo { modMod = "very", extWordLabel = (\x -> []), extWord = [], firstMod = ["will"], properness = False, modModNotFirst = True }
byVerbExampleInfo = ExampleInfo { modMod = "very", extWordLabel = (\x -> []), extWord = [], firstMod = ["by"], properness = False, modModNotFirst = True }

pastTense = (NTPast, NTPlus)
presentTense = (NTNow, NTPlus)
futureTense = (NTFuture, NTPlus)

buildExampleDatum :: Int -> [Text] -> [Text] -> ExampleTableId -> [Int] -> (ExampleTableId, Maybe ([ExampleWordLabel], [Text]))
buildExampleDatum :: _ tableId [] = (tableId, Nothing)
buildExampleDatum width nounMods nouns (SubjectTable sequenceId) groups = (SubjectTable sequenceId, example)
    where example = sentencify $ exampleDataWithDescriptor groups [0..(width-1)] nounExampleInfo nounMods nouns 0 modNexts
          modNexts = map (\n -> n == 1) $ padOrdinal width (encodeBitList sequenceId)
          sentencify Nothing = Nothing
          sentencify (Just (_, [])) = Nothing
          sentencify (Just (wLs, (firstWord:restWords))) = Just (wLs++completedwLs, (toTitle firstWord'):restWords++completedws)
          (completedwLs, completedws) = unzip $ map (\x -> (BackMatter,x)) (words "jumped over the back .")

buildExampleDatum width nounMods nouns (ObjectTable sequenceId) groups = (ObjectTable sequenceId, example)
    where (_, example) = buildExampleDatum width nounMods nouns (SubjectTable sequenceId) groups

buildExampleDatum width _ _ (ConjunctionTable) (g:rest) = (ConjunctionTable, example)
    where example = sentenceify $ conjunctionExampleData g conjunctions
          conjunctionExampleData _ [] = Nothing
          conjunctionExampleData 0 (c:_) = Just $ unzip (zip (map Other [0..]) c)
          conjunctionExampleData n (c:rest) = conjunctionExampleData (n-1) rest
          conjunctions = [[";"], [",","but"], [",","and"], [",","however",","], [",","since"]]
          sentencify Nothing = Nothing
          sentencify (Just (wLs, ws)) = Just (frontWLs++wLs++backWLs, frontWs++ws++backWs)
          (frontWLs, frontWs) = unzip $ map (\x -> (FrontMatter,x)) (words "She stopped it")
          (backWLs, backWs) = unzip $ map (\x -> (BackMatter,x)) (words "the car ran out of gas .")
          
buildExampleDatum width verbMods verbs (PrimaryAnalogyTable tense sequenceId) groups = (PrimaryAnalogyTable tense sequenceId, example)
    where example = sentencify $ exampleDataWithDescriptor groups [0..(width-1)] (selectInfo tense) verbMods verbs 0 modNexts
          --verbMods = words "quickly elaborately over up sedately never always fairly cleanly really"
          --verbs (NTPast, NTPlus) = words "jumped helped forced backed cluttered groaned held saw ran swam ate"
          --verbs (NTNow, NTPlus) = words "jumps helps forces backs clutters groans holds sees runs swims eats"
          --verbs (NTFuture, NTPlus) = if width == 0 then [] else words "jump help force back clutter groan hold see run swim eat"
          --verbs _ = verbs (NTNow, NTPlus)
          selectInfo (NTFuture, NTPlus) = verbFutureExampleInfo
          selectInfo _ = verbExampleInfo
          modNexts = map (\n -> n == 1) $ padOrdinal width (encodeBitList sequenceId)
          sentencify Nothing = Nothing
          sentencify (Just (wLs, ws)) = Just (frontWLs++wLs++backWLs, frontWs++ws++backWs)
          (frontWLs, frontWs) = unzip $ map (\x -> (FrontMatter,x)) (words "The fox")
          (backWLs, backWs) = unzip $ map (\x -> (BackMatter,x)) (words "the lazy dog .")
 
buildExampleDatum width verbMods verbs (AnalogyTable sequenceId) groups = (AnalogyTable tense sequenceId, example)
    where example = sentencify $ exampleDataWithDescriptor groups [0..(width-1)] verbExampleInfo verbMods verbs 0 modNexts
          modNexts = map (\n -> n == 1) $ padOrdinal width (encodeBitList sequenceId)
          sentencify Nothing = Nothing
          sentencify (Just (wLs, ws)) = Just (frontWLs++wLs++backWLs, frontWs++ws++backWs)
          (frontWLs, frontWs) = unzip $ map (\x -> (FrontMatter,x)) (words "The fox jumped over the dog")
          (backWLs, backWs) = unzip $ map (\x -> (BackMatter,x)) (words "the rocket pack .")
 
-- The fox jumped over the dog by quickly elaborately fairly cleanly really using the rocket pack.
-- The fox jumps over the dog by quickly elaborately fairly cleanly really using the rocket pack.
-- The fox will jump over the dog by quickly elaborately fairly cleanly leaping over the house.

randomize :: [Text] -> Rand StdGen [Text]
randomize ws = do
    let randomId :: Text -> Rand StdGen (Int, Text)
        randomId w = do
            i <- getRandom
            return (i, w)
    idWs <- mapM randomId ws
    let sorted = map (\(_,w) -> w) $ sortWith (\(i,_) -> i) idWs
    return sorted

getGroupsList :: Int -> [[Int]]
getGroupsList 5 = [[5],
                   [4,1],[1,4]
                   [3,2],[3,1,1],[1,3,1],[1,1,3],[2,3],
                   [2,2,1],[2,1,2],[1,2,2],
                   [2,1,1,1],[1,2,1,1],[1,1,2,1],[1,1,1,2],
                   [1,1,1,1,1]]
getGroupsList _ = []


buildSubjectExamples :: Rand StdGen TagSamples
buildSubjectExamples = do 
    let width = 5
        options = 2 ^ width
        groups = getGroupList width
    nounMods <- randomize $ words "jumpy young quick Charlie's happy sly small giant tailed fanged striped"
    nouns <- randomize $ words "fox colonel wines water skies fool vacuum patience lights box"
    let builder = buildExampleDatum width nounMods nouns
        sequences = map SubjectTable [0..(options-1)]
        data = map (map builder sequences) groups
        samples = map convertExampleDataToTagSample data
    return $ catMaybes samples

buildConjunctionExamples :: Rand StdGen TagSamples
buildConjunctionExamples = do 
    let width = 5
        groups = map (\v -> [v]) [0..width]
    let builder = buildExampleDatum width [] []
        data = map (builder ConjunctionTable) groups
        samples = map convertExampleDataToTagSample data
    return $ catMaybes samples

buildObjectExamples :: Rand StdGen TagSamples
buildObjectExamples = do 
    let width = 5
        options = 2 ^ width
        groups = getGroupList width
    nounMods <- randomize $ words "jumpy young quick Charlie's happy sly small giant tailed fanged striped"
    nouns <- randomize $ words "fox colonel wines water skies fool vacuum patience lights box"
    let builder = buildExampleDatum width nounMods nouns
        sequences = map ObjectTable [0..(options-1)]
        data = map (map builder sequences) groups
        samples = map convertExampleDataToTagSample data
    return $ catMaybes samples

buildAnalogyExamples :: Rand StdGen TagSamples
buildAnalogyExamples = do 
    let width = 5
        options = 2 ^ width
        groups = getGroupList width
    verbMods <- randomize $ words "quickly elaborately over up sedately never always fairly cleanly around"
    verbs <- randomize $ words "jumping helping forcing backing cluttering groaning holding seeing running swimming eating"
    let builder = buildExampleDatum width verbMods verbs
        sequences = map AnalogyTable [0..(options-1)]
        data = map (map builder sequences) groups
        samples = map convertExampleDataToTagSample data
    return $ catMaybes samples

buildPrimaryAnalogyExamples :: Rand StdGen TagSamples
buildPrimaryAnalogyExamples = do 
    let width = 5
        options = 2 ^ width
        groups = getGroupList width
        tenses = [pastTense, presentTense, futureTense]
    verbMods <- randomize $ words "quickly elaborately over up sedately never always fairly cleanly really"
    let verbs (NTPast, NTPlus) = do 
            ws <- randomize $ words "jumped helped forced backed cluttered groaned held saw ran swam ate"
            return ws
        verbs (NTNow, NTPlus) = do
            ws <- randomize $ words "jumps helps forces backs clutters groans holds sees runs swims eats"
            return ws
        verbs (NTFuture, NTPlus) = do
            ws <- randomize $ words "jump help force back clutter groan hold see run swim eat"
            return $ if width == 0 then [] else ws
        verbs _ = verbs (NTNow, NTPlus)
    let builder tense = buildExampleDatum width verbMods (verbs tense)
        sequences = map AnalogyTable [0..(options-1)]
        data = map (map (map builder tenses) sequences) groups
        samples = map convertExampleDataToTagSample data
    return $ catMaybes samples


buildPrimaryAnalogyExamples = undefined

showPossibleWords :: PossibleWords -> Text
showPossibleWords pws = tshow $ Map.toList (Map.map Vector.toList pws)

buildExamples :: Rand StdGen TagSamples
buildExamples = do
    subjectExamples <- buildSubjectExamples
    conjunctionExamples <- buildConjunctionExamples
    analogyExamples <- buildAnalogyExamples
    objectExamples <- buildObjectExamples
    return $ subjectExamples ++ conjunctionExamples ++ analogyExamples ++ objectExamples 

showExamples :: Rand StdGen Text
showExamples = tshow <$> buildExamples

writeFileExamples :: FilePath -> IO ()
writeFileExamples filename = do
    g <- newStdGen
    let t = evalRand showExamples g
    writeFileUtf8 filename t

readExamples :: Text -> Maybe TagSamples
readExamples = readMay

readFileExamples :: FilePath -> IO (Maybe TagSamples)
readFileExamples filename = do
    t <- readFileUtf8 filename
    return $ readExamples t

readPossibleWords :: Text -> Maybe PossibleWords
readPossibleWords t = possibleWords lists
    where lists = readMay t
          possibleWords Nothing = Nothing
          possibleWords (Just assocList) = Just $ Map.map Vector.fromList (Map.fromList assocList)

readFilePossibleWords :: FilePath -> IO (Maybe PossibleWords)
readFilePossibleWords filename = do
    t <- readFileUtf8 filename
    return $ readPossibleWords t

readWordGenerator :: FilePath -> IO (Maybe (WordGenerator Text))
readWordGenerator possibleWordsName = do
    possibleWords <- readFilePossibleWords possibleWordsName
    return $ buildWordGenerator possibleWords

indexedWordGenerator :: PossibleWords -> [(Int, PossibleWordIndex)] -> BuilderContext (LinearPhrase Text)
indexedWordGenerator _ [] = return []
indexedWordGenerator pws ((wId, pos):rest) = do
    indexedRest <- indexedWordGenerator pws rest
    let mDict = Map.lookup pos pws
        showWord Nothing = return $ "<" ++ tshow pos ++ " missing>"
        showWord (Just dict) = 
            let count = Vector.length dict
            in if count == 0 then return $ "<" ++ tshow pos ++ " empty>"
                             else do randId <- getRandomR (0,count-1)
                                     return $ dict Vector.! randId
    wordText <- showWord mDict
    return $ (wId, wordText):indexedRest 
    
    
buildIndexedWords :: Maybe Text -> [(Int,Text)] -> [(Int, PossibleWordIndex)]
buildIndexedWords _ [] = []
buildIndexedWords Nothing [(wId,wPOS)] = [(wId, OneWord wPOS)]
buildIndexedWords Nothing ((wId1,wPOS1):((wId2,wPOS2):rest)) = 
    (wId1, FirstWord wPOS1 wPOS2):buildIndexedWords (Just wPOS1) ((wId2,wPOS2):rest)
buildIndexedWords (Just wPOSPrior) [(wId,wPOS)] = [(wId, LastWord wPOSPrior wPOS)]
buildIndexedWords (Just wPOSPrior) ((wId,wPOS):((wIdNext,wPOSNext):rest)) = 
    (wId, MidWord wPOSPrior wPOS wPOSNext):buildIndexedWords (Just wPOS) ((wIdNext,wPOSNext):rest)
    
dataWordGenerator :: PossibleWords -> [(Int,Text)] -> BuilderContext (LinearPhrase Text)
dataWordGenerator pws ws = indexedWordGenerator pws $ buildIndexedWords Nothing ws
    
buildWordGenerator :: Maybe PossibleWords -> Maybe (WordGenerator Text)
buildWordGenerator (Just possibles) = Just $ dataWordGenerator possibles
buildWordGenerator Nothing = Nothing