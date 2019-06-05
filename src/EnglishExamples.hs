module EnglishExamples
    ( BuilderContext, nextM,
      AbstractTime (NTMinusInfinity, NTMinus, NTMinusDelta, NTPast, NTNow, NTFuture, NTPlusDelta, NTPlus, NTPlusInfinity), 
      Tense, Tenses,
      LinearPhrase, WordGenerator,
      BasicPhrase, PhraseCode(SubjectCode, ConjunctionCode, PrimaryAnalogyCode, AnalogyCode, ObjectCode), Linearizer, 
      encodeBitList, decodeBitList, decodeBitVector, padOrdinal,
      basicExampleData, exampleDataWithDescriptor, nounExampleInfo, verbExampleInfo, preVerbExampleInfo,

      maybeReadUtf8, showPossibleWords, showExamples, readFileExamples, 
      writeFileExamples, readWordGenerator, readLinearizer
      ) where

import ClassyPrelude
import qualified Data.Map.Strict as Map
import qualified Data.Vector as Vector
import qualified Data.Text as Text
import Data.Vector ((!?))
import System.IO (hPutStr)
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

data PossibleWordIndex = OnlyWord Text 
                       | FirstWord Text Text 
                       | MidWord Text Text Text 
                       | LastWord Text Text
                       | WordPair Text Text
                       | WordSingle Text deriving (Ord, Eq, Show, Read)
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
basicExampleData info _ noun proper modNum [] = if isProper then Just ([MainWord], [Text.toTitle noun]) else Just ([MainWord], [noun])
    where isProper = properness info && (proper || modNum == 0)
basicExampleData _ [] _ _ _ _ = Nothing
basicExampleData info (nm:restNounMods) noun proper modNum (modNext:rest) = addCurrent exampleDataRest
    where isProper = properness info && (proper || (modNum == 0 && modNext))
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
--                           groups             info         nounMods   nouns    modNexts  subPhrase
exampleDataWithDescriptor :: [Int] -> (Bool -> ExampleInfo) -> [Text] -> [Text] -> [Bool] -> Bool -> Maybe ([ExampleWordLabel], [Text])
exampleDataWithDescriptor [] _ _ _ _ _ = Nothing
exampleDataWithDescriptor _ _ _ [] _ _ = Nothing
exampleDataWithDescriptor (n:rest) info nounMods (noun:restNouns) modNexts subPhrase = 
    let groupNext = uncons $ drop (n-1) modNexts
        --If the nth modifier is compatible with splitting into a descriptor, it will be False followed by a non empty list
        canNotSplit = null rest || isNothing groupNext || groupNext == Just (False, []) || groupNext == Just (True, [])
        
        combineResults (Just (basicWLs, basicWs)) (Just (False, _)) (Just (descWLs, descWs)) =
            Just (basicWLs ++ extWordLabel infoS 0 ++ descWLs', basicWs ++ extWord infoS ++ descWs)
                where infoS = info subPhrase
                      maxNums = foldr foldMax (-1,0) -- bMe = 0 since it was used for extWordLabel
                      foldMax (Modifier mNum) (m,e) = if mNum > m then (mNum,e) else (m,e)
                      foldMax (Other eNum) (m,e) = if eNum > e then (m,eNum) else (m,e)
                      foldMax _ nums = nums
                      (bMm, bMe) = maxNums basicWLs
                      (dMm,_) = maxNums descWLs
                      updateModNum (Modifier m) = Modifier $ m + bMm+1
                      updateModNum (Other e) = Other $ e + bMe+1
                      updateModNum MainWord = Modifier $ dMm+1 + bMm+1
                      updateModNum other = other
                      descWLs' = map updateModNum descWLs
                      
        combineResults _ _ _ = Nothing

        basicRs = basicExampleData (info subPhrase) (take n nounMods) noun False 0 (take (if subPhrase then n-1 else n) modNexts)
        descriptorRs = exampleDataWithDescriptor rest info (drop n nounMods) restNouns (drop n modNexts) True
    in if canNotSplit
       then basicRs
       else combineResults basicRs groupNext descriptorRs

nounExampleInfo :: Bool -> ExampleInfo
nounExampleInfo _ = ExampleInfo { modMod = "very", extWordLabel = \x -> [Other x], extWord = ["of"], firstMod = ["the"], properness = True, modModNotFirst = False}
verbExampleInfo :: Bool -> ExampleInfo
verbExampleInfo _ = ExampleInfo { modMod = "very", extWordLabel = const [], extWord = [], firstMod = [], properness = False, modModNotFirst = False }
preVerbExampleInfo :: Text -> Bool -> ExampleInfo
preVerbExampleInfo preW False = ExampleInfo { modMod = "very", extWordLabel = const [], extWord = [], firstMod = [preW], properness = False, modModNotFirst = True }
preVerbExampleInfo _ True = ExampleInfo { modMod = "very", extWordLabel = const [], extWord = [], firstMod = [], properness = False, modModNotFirst = False }

pastTense :: Tense
pastTense = (NTPast, NTPast) -- "It had stopped it."
pastNowTense :: Tense
pastNowTense = (NTPast, NTNow)  -- "It stopped it."
pastFutureTense :: Tense
pastFutureTense = (NTPast, NTFuture) -- "It be stopping it."
presentTense :: Tense
presentTense = (NTNow, NTNow) -- "It stops it."
beginningTense :: Tense
beginningTense = (NTNow, NTFuture)  -- "It is stopping it."
futureTense :: Tense
futureTense = (NTFuture, NTFuture) -- "It will stop it."

adjustPattern :: ExampleInfo -> ([Int],[Bool]) -> ([Int],[Bool])
adjustPattern _ ([], modNexts) = ([], modNexts)
adjustPattern eInfo (g0:gRest, modNexts) | null (firstMod eInfo) = (g0:gRest, modNexts)
                                         | otherwise = (g0+1:gRest, False:modNexts)

buildExampleDatum :: Int -> [Text] -> [Text] -> ExampleTableId -> [Int] -> (ExampleTableId, Maybe ([ExampleWordLabel], [Text]))
buildExampleDatum _ _ _ tableId [] = (tableId, Nothing)
buildExampleDatum w nounMods nouns (SubjectTable sequenceId) gs = (SubjectTable sequenceId, example)
    where example = sentencify $ exampleDataWithDescriptor gs nounExampleInfo nounMods nouns modNexts False
          modNexts = map (== 1) $ padOrdinal (2 ^ w) (encodeBitList sequenceId)
          sentencify Nothing = Nothing
          sentencify (Just (_, [])) = Nothing
          sentencify (Just (wLs, firstWord:restWords)) = Just (wLs++completedwLs, Text.toTitle firstWord:restWords++completedws)
          (completedwLs, completedws) = unzip $ map (BackMatter,) (words "jumped over the back .")

buildExampleDatum w nounMods nouns (ObjectTable sequenceId) gs = (ObjectTable sequenceId, example)
    where example = sentencify $ exampleDataWithDescriptor gs nounExampleInfo nounMods nouns modNexts False
          modNexts = map (== 1) $ padOrdinal (2 ^ w) (encodeBitList sequenceId)
          sentencify Nothing = Nothing
          sentencify (Just (_, [])) = Nothing
          sentencify (Just (wLs, ws)) = Just (frontwLs++wLs++completedwLs, frontws++ws++completedws)
          (frontwLs, frontws) = unzip $ map (FrontMatter,) (words "The mouse jumped over")
          (completedwLs, completedws) = unzip $ map (BackMatter,) ["."]
          
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
    where example = sentencify $ exampleDataWithDescriptor gs' (selectInfo tense) verbMods verbs modNexts' False
          selectInfo (NTPast, NTPast) = preVerbExampleInfo "had"
          selectInfo (NTPast, NTNow) = verbExampleInfo
          selectInfo (NTPast, NTFuture) = preVerbExampleInfo "be"
          selectInfo (NTNow, NTNow) = verbExampleInfo
          selectInfo (NTNow, NTFuture) = preVerbExampleInfo "is"
          selectInfo (NTFuture, NTFuture) = preVerbExampleInfo "will"
          selectInfo _ = verbExampleInfo
          modNexts = map (== 1) $ padOrdinal (2 ^ w) (encodeBitList sequenceId)
          (gs', modNexts') = adjustPattern (selectInfo tense False) (gs, modNexts)
          sentencify Nothing = Nothing
          sentencify (Just (wLs, ws)) = Just (frontWLs++wLs++backWLs, frontWs++ws++backWs)
          (frontWLs, frontWs) = unzip $ map (FrontMatter,) (words "The fox")
          (backWLs, backWs) = unzip $ map (BackMatter,) (words "the lazy dog .")
 
buildExampleDatum w verbMods verbs (AnalogyTable sequenceId) gs = (AnalogyTable sequenceId, example)
    where example = sentencify $ exampleDataWithDescriptor gs' (preVerbExampleInfo "by") verbMods verbs modNexts' False
          modNexts = map (== 1) $ padOrdinal (2 ^ w) (encodeBitList sequenceId)
          (gs', modNexts') = adjustPattern (preVerbExampleInfo "by" False) (gs, modNexts)
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

buildSamples :: (Int -> ExampleTableId) -> [Text] -> [Text] -> [Text] -> Rand StdGen TagSamples
buildSamples tableId wordMods theWords moreWords = do
    wordMods' <- randomize wordMods
    theWords' <- randomize theWords
    moreWords' <- randomize moreWords
    let options :: Int
        options = 2 ^ width
        groups :: [[Int]]
        groups = getGroupsLists width
        builder = buildExampleDatum width wordMods' (take 1 theWords' ++ moreWords')
        sequences = map tableId [0..(options-1)] :: [ExampleTableId]
        sequenceGroups = concatMap (\s -> zip (repeat s) groups) sequences
        exampleData = map (uncurry builder) sequenceGroups
    return $ mapMaybe convertExampleDataToTagSample exampleData

getNounWords :: ([Text],[Text],[Text])
getNounWords = (nounMods, nouns, moreNouns)
    where nounMods = words "jumpy young quick Charlie's happy sly small giant tailed fanged striped"
          nouns = words "fox colonel wines water skies fool vacuum patience lights box"
          moreNouns = words "ceiling vapor chair transformer squirrel slug rock grain rat car"
    

buildSubjectExamples :: Rand StdGen TagSamples
buildSubjectExamples = buildSamples SubjectTable nounMods nouns moreNouns
    where (nounMods,nouns,moreNouns) = getNounWords

buildObjectExamples :: Rand StdGen TagSamples
buildObjectExamples = buildSamples ObjectTable nounMods nouns moreNouns
    where (nounMods,nouns,moreNouns) = getNounWords

buildConjunctionExamples :: Rand StdGen TagSamples
buildConjunctionExamples = return $ catMaybes samples
    where conjuctionGroups = map (:[]) [0..width]
          builder = buildExampleDatum width [] []
          exampleData = map (builder ConjunctionTable) conjuctionGroups
          samples = map convertExampleDataToTagSample exampleData

buildAnalogyExamples :: Rand StdGen TagSamples
buildAnalogyExamples = buildSamples AnalogyTable verbMods verbs moreMods
    where verbMods = words "quickly elaborately plainly usually sedately never always fairly cleanly not"
          verbs = words "jumping helping forcing backing cluttering groaning holding seeing running swimming eating"
          moreMods = words "reluctantly stupidly painfully ultimately stonily thoroughly scantily barely suddenly sparingly"

buildPrimaryAnalogyExamples :: Rand StdGen TagSamples
buildPrimaryAnalogyExamples = do 
    let verbMods = words "quickly elaborately plainly usually sedately never always fairly cleanly not"
        moreMods = words "reluctantly stupidly painfully ultimately stonily thoroughly scantily barely suddenly sparingly"
        tenses = [pastTense, pastNowTense, pastFutureTense, presentTense, beginningTense, futureTense] :: [Tense]
        verbs (NTPast, NTPast) = if width == 0 then return [] else 
            words "jumped helped forced backed cluttered groaned held saw ran swam ate"
        verbs (NTPast, NTNow) = verbs (NTPast, NTPast)
        verbs (NTPast, NTFuture) = if width == 0 then return [] else 
            words "jumping helping forcing backing cluttering groaning holding seeing running swimming eating"
        verbs (NTNow, NTNow) = words "jumps helps forces backs clutters groans holds sees runs swims eats"
        verbs (NTNow, NTFuture) = if width == 0 then return [] else 
            verbs (NTPast, NTFuture)
        verbs (NTFuture, NTFuture) = if width == 0 then return [] else 
            words "jump help force back clutter groan hold see run swim eat"
        verbs _ = verbs (NTNow, NTNow)
    let sample tense = buildSamples (PrimaryAnalogyTable tense) verbMods (verbs tense) moreMods
    samples <- mapM sample tenses
    return $ concat samples

showPossibleWords :: PossibleWords -> Text
showPossibleWords pws = tshow $ Map.toList (Map.map Vector.toList pws)

buildExamples :: Rand StdGen TagSamples
buildExamples = do
    s1 <- buildSubjectExamples
    s2 <- buildSubjectExamples
    c <- buildConjunctionExamples
    a1 <- buildAnalogyExamples
    a2 <- buildAnalogyExamples
    pa1 <- buildPrimaryAnalogyExamples
    pa2 <- buildPrimaryAnalogyExamples
    o1 <- buildObjectExamples
    o2 <- buildObjectExamples
    return $ s1 ++ s2 ++ c ++ a1 ++ a2 ++ pa1 ++ pa2 ++ o1 ++ o2

showExamples :: Rand StdGen Text
showExamples = tshow <$> buildExamples

writeFileExamples :: FilePath -> FilePath -> IO ()
writeFileExamples structFilename listsFilename = do
    g <- newStdGen
    let samples = evalRand buildExamples g
        toWordList (TagSample _ ws) = map toWord ws
        toWord (ExampleWord _ t) = t
        wsList = map toWordList samples
    writeFileUtf8 structFilename $ tshow samples
    writeFileUtf8 listsFilename $ tshow wsList


-----------------------------------------------------------------------
-- Implement reading in linearizer from POS tagged example sentences --

maybeReadUtf8 :: Read a => FilePath -> IO (Maybe a)
maybeReadUtf8 filename = do
    eResults <- tryIO $ readFileUtf8 filename
    let maybeResult (Left e) = do putStrLn $ "Error reading " ++ tshow filename ++ ": " ++ tshow e
                                  return Nothing
        maybeResult (Right t) = return $ readMay t
    maybeResult eResults

readFileExamples :: FilePath -> FilePath -> IO (Maybe TagSamples)
readFileExamples structFilename possFilename = do
    wordSamples <- maybeReadUtf8 structFilename
    posLists <- maybeReadUtf8 possFilename
    let samples = filledSamples wordSamples posLists
        filledSamples (Just ss) (Just ps) = zipWith (curry fillSample) ss ps
        filledSamples _ _ = []
        fillSample (TagSample tableId ews, poss) = TagSample tableId $ zipWith (curry fillWord) ews poss
        fillWord (ExampleWord wL _, t) = ExampleWord wL t
        matchError = matchSamples wordSamples posLists
        failMatch = matchError /= ""
        matchSamples (Just ss) (Just ps) = 
            let lps = length ps
                lss = length ss
            in if lss == lps 
               then concatMap matchSample (zip ss ps) 
               else "Length of struct " ++ tshow lss ++ " does not match " ++ tshow lps
        matchSamples Nothing _ = "Could not correctly read structure samples"
        matchSamples _ Nothing = "Could not correctly read part-of-speech samples"
        matchSample (TagSample sID ews, poss) = 
            let lews = length ews
                lposs = length poss
            in if lews == lposs
               then "" 
               else "Length of sample " ++ tshow sID ++ " , " ++ tshow lews ++ " does not match " ++ tshow lposs ++ "\n"
    if failMatch 
    then do
        hPutStr stderr $ show (matchError ++ "\n")
        return Nothing
    else do
        putStrLn "Successful read of file examples"
        return $ Just samples
        

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
        traceExampleWordLists = if Vector.null exampleWordLists 
                                then trace ("looking up " ++ show k ++ ":" ++ show exampleWordLists) exampleWordLists  
                                else exampleWordLists
        maxWL = Vector.length traceExampleWordLists
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


readLinearizer :: FilePath -> FilePath -> IO (Maybe (Linearizer Text))
readLinearizer structFilename possFilename = do
    mTagSamples <- readFileExamples structFilename possFilename
    return $ buildLinearizer mTagSamples


----------------------------------------------------------------
-- Implement reading in Word Generator from POS tagged corpus --

readFilePossibleWords :: FilePath -> IO (Maybe ([Text], PossibleWords))
readFilePossibleWords filename = do
    lists <- maybeReadUtf8 filename
    let possibleWords :: Maybe [(PossibleWordIndex,[Text])] -> Maybe ([Text], PossibleWords)
        possibleWords Nothing = Nothing
        possibleWords (Just assocList) = Just (allWords assocList, Map.map Vector.fromList (Map.fromList assocList))
        allWords :: [(PossibleWordIndex, [Text])] -> [Text]
        allWords = concatMap snd
    return $ possibleWords lists

indexedWordGenerator :: PossibleWords -> [(Int, PossibleWordIndex)] -> BuilderContext (LinearPhrase Text)
indexedWordGenerator _ [] = return []
indexedWordGenerator pws ((wId, pos):rest) = do
    indexedRest <- indexedWordGenerator pws rest
    let two (MidWord wPOSPrior wPOS _) = WordPair wPOSPrior wPOS
        two p = p
        one (MidWord _ wPOS _) = WordSingle wPOS
        one (LastWord _ wPOS) = WordSingle wPOS
        one (FirstWord wPOS _) = WordSingle wPOS
        one (OnlyWord wPOS) = WordSingle wPOS
        one (WordPair _ wPOS) = WordSingle wPOS
        one (WordSingle wPOS) = WordSingle wPOS
        mDict = Map.lookup pos pws <|>
                Map.lookup (two pos) pws <|> 
                Map.lookup (one pos) pws
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
buildIndexedWords Nothing [(wId,wPOS)] = [(wId, OnlyWord wPOS)]
buildIndexedWords Nothing ((wId1,wPOS1):((wId2,wPOS2):rest)) = 
    (wId1, FirstWord wPOS1 wPOS2):buildIndexedWords (Just wPOS1) ((wId2,wPOS2):rest)
buildIndexedWords (Just wPOSPrior) [(wId,wPOS)] = [(wId, LastWord wPOSPrior wPOS)]
buildIndexedWords (Just wPOSPrior) ((wId,wPOS):((wIdNext,wPOSNext):rest)) = 
    (wId, MidWord wPOSPrior wPOS wPOSNext):buildIndexedWords (Just wPOS) ((wIdNext,wPOSNext):rest)
    
dataWordGenerator :: PossibleWords -> [(Int,Text)] -> BuilderContext (LinearPhrase Text)
dataWordGenerator pws ws = indexedWordGenerator pws $ buildIndexedWords Nothing ws
    
buildWordGenerator :: Maybe ([Text],PossibleWords) -> Maybe ([Text], WordGenerator Text)
buildWordGenerator (Just (dictionary, possibles)) = Just (dictionary, dataWordGenerator possibles)
buildWordGenerator Nothing = Nothing

readWordGenerator :: FilePath -> IO (Maybe ([Text], WordGenerator Text))
readWordGenerator possibleWordsName = do
    possibleWords <- readFilePossibleWords possibleWordsName
    return $ buildWordGenerator possibleWords

