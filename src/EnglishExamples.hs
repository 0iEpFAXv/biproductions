module EnglishExamples
    ( BuilderContext, nextM,
      AbstractTime (NTMinusInfinity, NTMinus, NTMinusDelta, NTPast, NTNow, NTFuture, NTPlusDelta, NTPlus, NTPlusInfinity), 
      Tense, Tenses,
      LinearPhrase, WordGenerator,
      BasicPhrase, PhraseCode(SubjectCode, ConjunctionCode, PrimaryAnalogyCode, AnalogyCode, ObjectCode), Linearizer, 
      encodeBitList, decodeBitList, decodeBitVector, padOrdinal,
      basicExampleData, nounExampleInfo, verbExampleInfo, verbFutureExampleInfo, byVerbExampleInfo,

      showPossibleWords, showExamples, readExamples, readFileExamples, 
      writeFileExamples, readWordGenerator, readLinearizer
      ) where

import ClassyPrelude
import qualified Data.Map.Strict as Map
import qualified Data.Vector as Vector
import qualified Data.Text as Text
import Data.Vector ((!?))
import Control.Monad.Random.Lazy hiding (replicateM)
import Control.Monad.Trans.State.Lazy

data AbstractTime = NTMinusInfinity | NTPast | NTMinus | NTMinusDelta
                  | NTNow 
                  | NTPlusDelta | NTPlus | NTFuture | NTPlusInfinity 
                  deriving (Eq, Ord, Read, Show)
type Tense = (AbstractTime, AbstractTime)
type Tenses = Vector Tense

type BuilderContext a = RandT StdGen (State Int) a

type LinearPhrase a = [(Int,a)]
type WordGenerator a = LinearPhrase a -> BuilderContext (LinearPhrase Text)

-- BasicPhrase has list of modifiers, modifiees, modified word, and list of extra words for structural grammar
type BasicPhrase = ([Int],[Int],Int,[Int])
data PhraseCode = SubjectCode Int BasicPhrase 
                | ConjunctionCode BasicPhrase
                | PrimaryAnalogyCode Tense Int BasicPhrase
                | AnalogyCode Int BasicPhrase
                | ObjectCode Int BasicPhrase
type Linearizer a = PhraseCode -> BuilderContext (LinearPhrase a)

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
                    | ObjectTable Int deriving (Eq, Ord, Show, Read)
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


-------------------------------------------------------
-- Implement writing out example sentences to a file --

convertExampleDataToTagSample :: (ExampleTableId, Maybe ([ExampleWordLabel], [Text])) -> Maybe TagSample
convertExampleDataToTagSample (tableId, Just (wLs, ws)) = Just $ TagSample tableId (zipWith ExampleWord wLs ws)
convertExampleDataToTagSample (_, Nothing) = Nothing

--                  info           nounMods  noun   proper  modNum  modNexts
basicExampleData :: ExampleInfo -> [Text] -> Text -> Bool -> Int -> [Bool] -> Maybe ([ExampleWordLabel], [Text])
basicExampleData _ _ noun proper _ [] = if proper then Just ([MainWord], [Text.toTitle noun]) else Just ([MainWord], [noun])
basicExampleData _ [] _ _ _ _ = Nothing
basicExampleData info (nm:restNounMods) noun proper modNum (modNext:rest) = addCurrent exampleDataRest
    where isProper = properness info && proper || (modNum == 0 && modNext)
          nm' = if modNum == 0 && not isProper
                then maybe nm fst (uncons (firstMod info))
                else nm
          m = if not modNext || null rest || (modNum == 0 && modModNotFirst info)
              then nm'
              else modMod info
          impossible = modNum == 0 && modModNotFirst info && modNext
          addCurrent Nothing = Nothing
          addCurrent (Just (wordLabelRest, wordsRest)) | impossible = Nothing
                                                       | otherwise = Just (Modifier modNum:wordLabelRest, m:wordsRest)
          exampleDataRest = basicExampleData info restNounMods noun isProper (modNum+1) rest

data ExampleInfo = ExampleInfo { modMod :: Text
                               , extWordLabel :: Int -> [ExampleWordLabel]
                               , extWord :: [Text]
                               , firstMod :: [Text]
                               , properness :: Bool
                               , modModNotFirst :: Bool}

-- The first argument is a list of modifier counts to scoop up and build with basicNounExampleData
--                           groups  extras     info         nounMods   nouns    modNum  modNexts   
exampleDataWithDescriptor :: [Int] -> [Int] -> ExampleInfo -> [Text] -> [Text] -> Int -> [Bool] -> Maybe ([ExampleWordLabel], [Text])
exampleDataWithDescriptor [] _ _ _ _ _ _ = Nothing
exampleDataWithDescriptor _ [] _ _ _ _ _ = Nothing
exampleDataWithDescriptor _ _ _ [] _ _ _ = Nothing
exampleDataWithDescriptor _ _ _ _ [] _ _ = Nothing
exampleDataWithDescriptor (0:rest) exts info nounMods nouns modNum modNexts =
    exampleDataWithDescriptor rest exts info nounMods nouns modNum modNexts
exampleDataWithDescriptor (n:rest) (ext:restExt) info nounMods (noun:restNouns) modNum modNexts = 
    let groupNext = uncons $ drop (n-1) modNexts
        --If the nth modifier is compatible with splitting into a descriptor, it will be False followed by a non empty list
        canNotSplit = isNothing groupNext || groupNext == Just (False, []) || groupNext == Just (True, [])
        
        combineResults (Just (basicWLs, basicWs)) (Just (False, _)) (Just (descWLs, descWs)) =
            Just (basicWLs ++ extWordLabel info ext ++ descWLs', basicWs ++ extWord info ++ descWs)
                where maxModNum = foldr foldMax (modNum+n) descWLs
                      foldMax (Modifier x) y = if x > y then x else y 
                      foldMax _ y = y
                      descWLs' = map (\MainWord -> Modifier maxModNum) descWLs
        combineResults _ _ _ = Nothing

        basicRs = basicExampleData info (firstMod info ++ take n nounMods) noun False modNum (take n modNexts)
        descriptorRs = exampleDataWithDescriptor rest restExt info (drop n nounMods) restNouns (modNum+n) (drop (n+1) modNexts)
    in if canNotSplit
       then basicRs
       else combineResults basicRs groupNext descriptorRs

nounExampleInfo :: ExampleInfo
nounExampleInfo = ExampleInfo { modMod = "very", extWordLabel = \x -> [Other x], extWord = ["of"], firstMod = ["the"], properness = True, modModNotFirst = False}
verbExampleInfo :: ExampleInfo
verbExampleInfo = ExampleInfo { modMod = "very", extWordLabel = const [], extWord = [], firstMod = [], properness = False, modModNotFirst = False }
verbFutureExampleInfo :: ExampleInfo
verbFutureExampleInfo = ExampleInfo { modMod = "very", extWordLabel = const [], extWord = [], firstMod = ["will"], properness = False, modModNotFirst = True }
byVerbExampleInfo :: ExampleInfo
byVerbExampleInfo = ExampleInfo { modMod = "very", extWordLabel = const [], extWord = [], firstMod = ["by"], properness = False, modModNotFirst = True }

pastTense :: Tense
pastTense = (NTPast, NTPlus)
presentTense :: Tense
presentTense = (NTNow, NTPlus)
futureTense :: Tense
futureTense = (NTFuture, NTPlus)

buildExampleDatum :: Int -> [Text] -> [Text] -> ExampleTableId -> [Int] -> (ExampleTableId, Maybe ([ExampleWordLabel], [Text]))
buildExampleDatum _ _ _ tableId [] = (tableId, Nothing)
buildExampleDatum w nounMods nouns (SubjectTable sequenceId) gs = (SubjectTable sequenceId, example)
    where example = sentencify $ exampleDataWithDescriptor gs [0..(w-1)] nounExampleInfo nounMods nouns 0 modNexts
          modNexts = map (== 1) $ padOrdinal w (encodeBitList sequenceId)
          sentencify Nothing = Nothing
          sentencify (Just (_, [])) = Nothing
          sentencify (Just (wLs, firstWord:restWords)) = Just (wLs++completedwLs, Text.toTitle firstWord:restWords++completedws)
          (completedwLs, completedws) = unzip $ map (BackMatter,) (words "jumped over the back .")

buildExampleDatum w nounMods nouns (ObjectTable sequenceId) gs = (ObjectTable sequenceId, example)
    where (_, example) = buildExampleDatum w nounMods nouns (SubjectTable sequenceId) gs

buildExampleDatum _ _ _ ConjunctionTable (g:_) = (ConjunctionTable, example)
    where example = sentencify $ conjunctionExampleData g conjunctions
          conjunctionExampleData _ [] = Nothing
          conjunctionExampleData 0 (c:_) = Just $ unzip (zip (map Other [0..]) c)
          conjunctionExampleData n (_:rest) = conjunctionExampleData (n-1) rest
          conjunctions = [[";"], [",","but"], [",","and"], [",","however",","], [",","since"]]
          sentencify Nothing = Nothing
          sentencify (Just (wLs, ws)) = Just (frontWLs++wLs++backWLs, frontWs++ws++backWs)
          (frontWLs, frontWs) = unzip $ map (FrontMatter,) (words "She stopped it")
          (backWLs, backWs) = unzip $ map (BackMatter,) (words "the car ran out of gas .")
          
buildExampleDatum w verbMods verbs (PrimaryAnalogyTable tense sequenceId) gs = (PrimaryAnalogyTable tense sequenceId, example)
    where example = sentencify $ exampleDataWithDescriptor gs [0..(w-1)] (selectInfo tense) verbMods verbs 0 modNexts
          selectInfo (NTFuture, NTPlus) = verbFutureExampleInfo
          selectInfo _ = verbExampleInfo
          modNexts = map (== 1) $ padOrdinal w (encodeBitList sequenceId)
          sentencify Nothing = Nothing
          sentencify (Just (wLs, ws)) = Just (frontWLs++wLs++backWLs, frontWs++ws++backWs)
          (frontWLs, frontWs) = unzip $ map (FrontMatter,) (words "The fox")
          (backWLs, backWs) = unzip $ map (BackMatter,) (words "the lazy dog .")
 
buildExampleDatum w verbMods verbs (AnalogyTable sequenceId) gs = (AnalogyTable sequenceId, example)
    where example = sentencify $ exampleDataWithDescriptor gs [0..(w-1)] byVerbExampleInfo verbMods verbs 0 modNexts
          modNexts = map (== 1) $ padOrdinal w (encodeBitList sequenceId)
          sentencify Nothing = Nothing
          sentencify (Just (wLs, ws)) = Just (frontWLs++wLs++backWLs, frontWs++ws++backWs)
          (frontWLs, frontWs) = unzip $ map (FrontMatter,) (words "The fox jumped over the dog")
          (backWLs, backWs) = unzip $ map (BackMatter,) (words "the rocket pack .")
 
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
    let sorted = map snd $ sortWith fst idWs
    return sorted

getGroupsLists :: Int -> [[Int]]
getGroupsLists 5 = [[5],
                   [4,1],[1,4],
                   [3,2],[3,1,1],[1,3,1],[1,1,3],[2,3],
                   [2,2,1],[2,1,2],[1,2,2],
                   [2,1,1,1],[1,2,1,1],[1,1,2,1],[1,1,1,2],
                   [1,1,1,1,1]]
getGroupsLists _ = []

width :: Int
width = 5

buildSamples :: (Int -> ExampleTableId) -> [Text] -> [Text] -> Rand StdGen TagSamples
buildSamples tableId wordMods theWords = do
    wordMods' <- randomize wordMods
    theWords' <- randomize theWords
    let options :: Int
        options = 2 ^ width
        groups :: [[Int]]
        groups = getGroupsLists width
        builder = buildExampleDatum width wordMods' theWords'
        sequences = map tableId [0..(options-1)] :: [ExampleTableId]
        sequenceGroups = concatMap (\s -> zip (repeat s) groups) sequences
        exampleData = map (uncurry builder) sequenceGroups
    return $ mapMaybe convertExampleDataToTagSample exampleData

buildSubjectExamples :: Rand StdGen TagSamples
buildSubjectExamples = buildSamples SubjectTable nounMods nouns
    where nounMods = words "jumpy young quick Charlie's happy sly small giant tailed fanged striped"
          nouns = words "fox colonel wines water skies fool vacuum patience lights box"

buildConjunctionExamples :: Rand StdGen TagSamples
buildConjunctionExamples = return $ catMaybes samples
    where conjuctionGroups = map (:[]) [0..width]
          builder = buildExampleDatum width [] []
          exampleData = map (builder ConjunctionTable) conjuctionGroups
          samples = map convertExampleDataToTagSample exampleData

buildObjectExamples :: Rand StdGen TagSamples
buildObjectExamples = buildSamples ObjectTable nounMods nouns
    where nounMods = words "jumpy young quick Charlie's happy sly small giant tailed fanged striped"
          nouns = words "fox colonel wines water skies fool vacuum patience lights box"

buildAnalogyExamples :: Rand StdGen TagSamples
buildAnalogyExamples = buildSamples AnalogyTable verbMods verbs
    where verbMods = words "quickly elaborately over up sedately never always fairly cleanly around"
          verbs = words "jumping helping forcing backing cluttering groaning holding seeing running swimming eating"

buildPrimaryAnalogyExamples :: Rand StdGen TagSamples
buildPrimaryAnalogyExamples = do 
    let verbMods = words "quickly elaborately over up sedately never always fairly cleanly really"
        tenses = [pastTense, presentTense, futureTense] :: [Tense]
        verbs (NTPast, NTPlus) = words "jumped helped forced backed cluttered groaned held saw ran swam ate"
        verbs (NTNow, NTPlus) = words "jumps helps forces backs clutters groans holds sees runs swims eats"
        verbs (NTFuture, NTPlus) = if width == 0 then return [] else 
            words "jump help force back clutter groan hold see run swim eat"
        verbs _ = verbs (NTNow, NTPlus)
    let sample tense = buildSamples (PrimaryAnalogyTable tense) verbMods (verbs tense)
    samples <- mapM sample tenses
    return $ concat samples

showPossibleWords :: PossibleWords -> Text
showPossibleWords pws = tshow $ Map.toList (Map.map Vector.toList pws)

buildExamples :: Rand StdGen TagSamples
buildExamples = do
    subjectExamples <- buildSubjectExamples
    conjunctionExamples <- buildConjunctionExamples
    analogyExamples <- buildAnalogyExamples
    primaryAnalogyExamples <- buildPrimaryAnalogyExamples
    objectExamples <- buildObjectExamples
    return $ subjectExamples ++ conjunctionExamples ++ analogyExamples ++ primaryAnalogyExamples ++ objectExamples 

showExamples :: Rand StdGen Text
showExamples = tshow <$> buildExamples

writeFileExamples :: FilePath -> IO ()
writeFileExamples filename = do
    g <- newStdGen
    let t = evalRand showExamples g
    writeFileUtf8 filename t


-----------------------------------------------------------------------
-- Implement reading in linearizer from POS tagged example sentences --

readExamples :: Text -> Maybe TagSamples
readExamples = readMay

readFileExamples :: FilePath -> IO (Maybe TagSamples)
readFileExamples filename = do
    t <- readFileUtf8 filename
    return $ readExamples t

-- BasicPhrase has list of modifiers, modifiees, modified word, and list of extra words for structural grammar
--type BasicPhrase = ([Int],[Int],Int,[Int])
--data PhraseCode = SubjectCode Int BasicPhrase 
--                | ConjunctionCode BasicPhrase
--                | PrimaryAnalogyCode Tense Int BasicPhrase
--                | AnalogyCode Int BasicPhrase
--                | ObjectCode Int BasicPhrase
--type Linearizer a = PhraseCode -> BuilderContext (LinearPhrase a)

--data ExampleWordLabel = FrontMatter | Modifier ModifierId | MainWord | Other Int | BackMatter deriving (Eq, Show, Read)
--data ExampleWord = ExampleWord ExampleWordLabel Text deriving (Eq, Show, Read)
--type ExampleWords = [ExampleWord]
--data ExampleTableId = SubjectTable Int 
--                    | ConjunctionTable 
--                    | PrimaryAnalogyTable Tense Int 
--                    | AnalogyTable Int
--                    | ObjectTable Int deriving (Eq, Show, Read)
--data TagSample = TagSample ExampleTableId ExampleWords deriving (Eq, Show, Read)
--type TagSamples = [TagSample]

linearizeBasicWords :: Map ExampleTableId (Vector ExampleWords) -> ExampleTableId -> BasicPhrase -> BuilderContext (LinearPhrase Text)
linearizeBasicWords tables k (ms,_,w,exts) = do
    let exampleWordLists = Map.findWithDefault Vector.empty k tables
        maxWL = Vector.length exampleWordLists
    selection <- getRandomR (0,maxWL-1)
    let wl = maybe [] (mapMaybe filterFn) $ exampleWordLists !? selection
        filterFn (ExampleWord (Modifier mId) pos) = Vector.fromList (zip ms (repeat pos)) !? mId
        filterFn (ExampleWord MainWord pos) = Just (w,pos)
        filterFn (ExampleWord (Other extId) pos) = Vector.fromList (zip exts (repeat pos)) !? extId
        filterFn (ExampleWord FrontMatter _) = Nothing
        filterFn (ExampleWord BackMatter _) = Nothing
    return wl

linearizeWords :: Map ExampleTableId (Vector ExampleWords) -> PhraseCode -> BuilderContext (LinearPhrase Text)
linearizeWords tables (SubjectCode k bp) = linearizeBasicWords tables (SubjectTable k) bp
linearizeWords tables (ConjunctionCode bp) = linearizeBasicWords tables ConjunctionTable bp
linearizeWords tables (PrimaryAnalogyCode t k bp) = linearizeBasicWords tables (PrimaryAnalogyTable t k) bp
linearizeWords tables (AnalogyCode k bp) = linearizeBasicWords tables (AnalogyTable k) bp
linearizeWords tables (ObjectCode k bp) = linearizeBasicWords tables (ObjectTable k) bp

buildLinearizer :: Maybe TagSamples -> Maybe (Linearizer Text)
buildLinearizer Nothing = Nothing
buildLinearizer (Just samples) = Just $ linearizeWords tables
    where tables = Vector.foldl mapAccum Map.empty $ Vector.fromList samples
          mapAccum :: Map ExampleTableId (Vector ExampleWords) -> TagSample -> Map ExampleTableId (Vector ExampleWords)
          mapAccum m (TagSample k ws) = Map.alter alterFn k m
              where alterFn Nothing = Just $ Vector.singleton ws
                    alterFn (Just priorWs) = Just $ Vector.cons ws priorWs


readLinearizer :: FilePath -> IO (Maybe (Linearizer Text))
readLinearizer linearizerName = do
    mTagSamples <- readFileExamples linearizerName
    return $ buildLinearizer mTagSamples


----------------------------------------------------------------
-- Implement reading in Word Generator from POS tagged corpus --

readPossibleWords :: Text -> Maybe PossibleWords
readPossibleWords t = possibleWords lists
    where lists = readMay t
          possibleWords Nothing = Nothing
          possibleWords (Just assocList) = Just $ Map.map Vector.fromList (Map.fromList assocList)

readFilePossibleWords :: FilePath -> IO (Maybe PossibleWords)
readFilePossibleWords filename = do
    t <- readFileUtf8 filename
    return $ readPossibleWords t

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

readWordGenerator :: FilePath -> IO (Maybe (WordGenerator Text))
readWordGenerator possibleWordsName = do
    possibleWords <- readFilePossibleWords possibleWordsName
    return $ buildWordGenerator possibleWords

