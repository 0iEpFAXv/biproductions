module BiProductionsLib
    ( testMessage,
      InputWord(InputText, InputTime, InputRelativeTime), InputSentence,
      TenseId, Modifier(Modifier), Analogy(Analogy), Sentence(Sentence),
      OneHotCaps(ohcWords, ohcTenses), defaultOneHotCaps, wordOrdinals, padOrdinals,
      mapMSentence,
      encodeOneHotInput, encodeOneHotOutput, encodeOneHotOutputWord, decodeOneHotOutput, getBitGroups,
      decodeOneHotTense, decodeEncodeOneHotOutput,
      TenseBitGroup(TenseBitGroup), WordBitGroup(WordBitGroup), buildTenses,
      BasicPhrase, Phrase(SubjectP, ConjunctionP, AnalogyObjectP),
      PhraseCode(SubjectCode, ConjunctionCode, AnalogyObjectCode), 
      generateSentence, Linearizer, 
      
      -- From EnglishExamples
      BuilderContext, nextM,
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
import qualified Data.Char as Char
import Control.Monad.Random.Lazy hiding (replicateM)
import Control.Monad.Trans.State.Lazy
import EnglishExamples

testMessage :: Text
testMessage = "Hello World"

data InputWord = InputText Text | InputTime Text UTCTime | InputRelativeTime Text Double 
                 deriving (Eq, Read, Show)
type InputSentence = Vector InputWord

type TenseId = Int
data Modifier a = Modifier a a deriving (Eq, Read, Show)
instance Functor Modifier where
    fmap f (Modifier m w) = Modifier (f m) (f w) 
type Modifiers a = Vector (Modifier a)
data Analogy a = Analogy TenseId a a a deriving (Eq, Read, Show)
instance Functor Analogy where
    fmap f (Analogy tId s o a) = Analogy tId (f s) (f o) (f a)
type Analogies a = Vector (Analogy a)
data Sentence a = Sentence Tenses (Modifiers a) (Analogies a) deriving (Eq, Read, Show)
instance Functor Sentence where
    fmap f (Sentence ts ms as) = Sentence ts (fmap (fmap f) ms) (fmap (fmap f) as)
-- someting :: Monoid m => (m a -> b) -> Sentence (m a) -> m (Sentence b)

data OneHotCaps = OneHotCaps { ohcWords :: Int, ohcTenses :: Int } deriving (Eq, Read, Show)
defaultOneHotCaps :: OneHotCaps
defaultOneHotCaps = OneHotCaps { ohcWords = 64, ohcTenses = 2}

type WordOrdinals = Text -> ([Int], [Int])

mapMSentence :: Monad m => (a -> m b) -> Sentence a -> m (Sentence b)
mapMSentence f (Sentence ts ms as) = do
    let mf (Modifier oM oW) = do
            m <- f oM
            w <- f oW
            return $ Modifier m w
        af (Analogy tId oS oO oA) = do
            s <- f oS
            o <- f oO
            a <- f oA
            return $ Analogy tId s o a
    fMs <- Vector.mapM mf ms
    fAs <- Vector.mapM af as
    return $ Sentence ts fMs fAs
          
wordOrdinals :: [Text] -> WordOrdinals
wordOrdinals dictionary = lookUps
    where binaryValues = map encodeBitList [1..]
          indexedSet l = Map.fromAscList $ zip (sort l) binaryValues
          s = indexedSet (map toLower dictionary)
          rs = indexedSet (map (toLower . reverse) dictionary)
          lookUp :: Text -> Map Text [Int] -> [Int]
          lookUp w d = maybe [0] snd (Map.lookupLE w d)
          lookUps w = (lookUp lw s, lookUp (reverse lw) rs) where lw = toLower w

padOrdinals :: Int -> Int -> WordOrdinals -> WordOrdinals
padOrdinals fl sl ordinals = newOrdinals
    where newOrdinals = pad . ordinals
          pad (f,s) = (padOrdinal fl f, padOrdinal sl s)

encodeOneHotInput :: [Text] -> OneHotCaps -> InputSentence -> Either Text [Int]
encodeOneHotInput dictionary = encodeOneHotInputWords (padOrdinals 16 8 (wordOrdinals dictionary)) 0

encodeOneHotInputWords :: WordOrdinals -> Int -> OneHotCaps -> InputSentence -> Either Text [Int]
encodeOneHotInputWords ordinals wordId caps s 
    | wordId >= ohcWords caps = return []
    | otherwise = do
        let word = getWordFromSentence s wordId
            paddedWord = padOrdinal (ohcWords caps) $ encodeBitList wordId
        wordBlock <- encodeOneHotInputWord ordinals word paddedWord
        rest <- encodeOneHotInputWords ordinals (wordId+1) caps s
        return (wordBlock ++ rest)

getWordText :: Maybe InputWord -> Text
getWordText Nothing = ""
getWordText (Just (InputText t)) = t
getWordText (Just (InputTime t _)) = t
getWordText (Just (InputRelativeTime t _)) = t

getWordFromSentence :: InputSentence -> Int -> Maybe InputWord
getWordFromSentence is wordId = is Vector.!? wordId

--   wordCap blocks of:
--     1 bit: is present
--     1 bit: is capitalized
--     1 bit: is time
--     16 bits: most significant bits of word ordinal sorted forward
--     8 bits: most significant bits of word ordinal sorted reverse
--     log_2 wordCap bits: word id
encodeOneHotInputWord :: WordOrdinals -> Maybe InputWord -> [Int]
                         -> Either Text [Int] 
encodeOneHotInputWord ordinals mInputWord paddedWord = do
    let wordText = getWordText mInputWord
        present = if wordText == "" then 0 else 1
        capitalB = maybe False (Char.isUpper . fst) $ uncons wordText
        capital = if capitalB then 1 else 0
        isTime (Just (InputTime _ _)) = 1
        isTime (Just (InputRelativeTime _ _)) = 1
        isTime _ = 0
        time = isTime mInputWord
        (ordF, ordR) = ordinals wordText
    return $ [present,capital,time] ++ ordF ++ ordR ++ paddedWord
   

encodeOneHotOutput :: OneHotCaps -> Sentence Int -> Either Text [Int]
encodeOneHotOutput caps s = do
    tenses <- encodeOneHotTenses caps s 0
    encodedWords <- encodeOneHotOutputWords caps s 0
    return $ tenses ++ encodedWords
    
--   tensesCap blocks of:
--     7 bit: select AbstractTime start
--     7 bit: select AbstractTime end
encodeOneHotTenses :: OneHotCaps -> Sentence Int -> Int -> Either Text [Int]
encodeOneHotTenses caps s tenseId
    | tenseId >= ohcTenses caps = return []
    | otherwise = do
        let getSentenceTense (Sentence tenses _ _) = tenses Vector.!? tenseId
            propMaybe (Just (t1, t2)) = (Just t1, Just t2)
            propMaybe _ = (Nothing, Nothing)
            (mTenseStart, mTenseEnd) = propMaybe $ getSentenceTense s
            tenseStart = encodeOneHotTense mTenseStart
            tenseEnd = encodeOneHotTense mTenseEnd
        rest <- encodeOneHotTenses caps s (tenseId+1)
        return $ tenseStart ++ tenseEnd ++ rest
    
encodeOneHotTense :: Maybe AbstractTime -> [Int]
encodeOneHotTense Nothing =                [0,0,0,0,0,0,0,0,0]
encodeOneHotTense (Just NTMinusInfinity) = [1,0,0,0,0,0,0,0,0]
encodeOneHotTense (Just NTMinus) =         [0,1,0,0,0,0,0,0,0]
encodeOneHotTense (Just NTMinusDelta) =    [0,0,1,0,0,0,0,0,0]
encodeOneHotTense (Just NTPast) =          [0,0,0,1,0,0,0,0,0]
encodeOneHotTense (Just NTNow) =           [0,0,0,0,1,0,0,0,0]
encodeOneHotTense (Just NTFuture) =        [0,0,0,0,0,1,0,0,0]
encodeOneHotTense (Just NTPlusDelta) =     [0,0,0,0,0,0,1,0,0]
encodeOneHotTense (Just NTPlus) =          [0,0,0,0,0,0,0,1,0]
encodeOneHotTense (Just NTPlusInfinity) =  [0,0,0,0,0,0,0,0,1]

decodeOneHotTense :: Vector Int -> Maybe AbstractTime
decodeOneHotTense bits = decodeOneHotTenseId $ Vector.findIndex (== 1) bits
decodeOneHotTenseId :: Maybe Int -> Maybe AbstractTime
decodeOneHotTenseId (Just 0) = Just NTMinusInfinity
decodeOneHotTenseId (Just 1) = Just NTMinus
decodeOneHotTenseId (Just 2) = Just NTMinusDelta
decodeOneHotTenseId (Just 3) = Just NTPast
decodeOneHotTenseId (Just 4) = Just NTNow
decodeOneHotTenseId (Just 5) = Just NTFuture
decodeOneHotTenseId (Just 6) = Just NTPlusDelta
decodeOneHotTenseId (Just 7) = Just NTPlus
decodeOneHotTenseId (Just 8) = Just NTPlusInfinity
decodeOneHotTenseId _ =        Nothing

--NTMinusInfinity | NTMinus | NTMinusDelta 
--                  | NTRelative Double | NTNow | NTDate UTCTime 
--                  | NTPlusDelta | NTPlus | NTPlusInfinity 

--   wordCap blocks of:
--     1 bit: is present
--     1 bit: is subject
--     1 bit: is object
--     1 bit: is analogy
--     log_2 tensesCap bits: select tense
--     log_2 wordCap bits: referent word id
--       Notes: For modifier, referent word id refers to modifier, object or analogy that is being modified
--              For subject, referent word id refers to subject
--              For object, referent word id refers to subject
--              For analogy, referent word id refers to object
encodeOneHotOutputWords :: OneHotCaps -> Sentence Int -> Int -> Either Text [Int]
encodeOneHotOutputWords caps s wordId
    | wordId >= ohcWords caps = return []
    | otherwise = do
        wordBlock <- encodeOneHotOutputWord caps s wordId
        rest <- encodeOneHotOutputWords caps s (wordId+1)
        return (wordBlock ++ rest)
        
encodeOneHotOutputWord :: OneHotCaps -> Sentence Int -> Int -> Either Text [Int]
encodeOneHotOutputWord caps (Sentence _ modifiers analogies) wordId = 
    let present = if Vector.any isModifier modifiers then 1 else 0
        
        isSubject (Analogy _ s _ _) = s == wordId
        isAnySubject = Vector.any isSubject analogies
        subject = if isAnySubject then 1 else 0
        
        isObject (Analogy _ _ o _) = o == wordId
        isAnyObject = Vector.any isObject analogies
        object = if isAnyObject then 1 else 0

        isAnalogy (Analogy _ _ _ a) = a == wordId
        isAnyAnalogy = Vector.any isAnalogy analogies
        analogy = if isAnyAnalogy then 1 else 0
        
        findTense = maybe 0 (\(Analogy tId _ _ _) -> tId) $ Vector.find isAnalogy analogies
        tenseId = if isAnyAnalogy then findTense else 0
        tense = padOrdinal (ohcTenses caps) (encodeBitList tenseId)

        getReferent Nothing = wordId 
        getReferent (Just (Modifier _ r)) = r
        isModifier (Modifier m _) = m == wordId
        referentId = getReferent $ Vector.find isModifier modifiers
        referent = padOrdinal (ohcWords caps) (encodeBitList referentId)
    in return $ [present, subject, object, analogy] ++ tense ++ referent

data WordBitGroup = WordBitGroup Int Int Int Int Int (Vector Int) (Vector Int) deriving (Eq, Show, Read)
data TenseBitGroup = TenseBitGroup (Vector Int) (Vector Int) deriving (Eq, Show, Read)

bitSplitWE :: Int -> Vector Int -> Either Text (Vector Int, Vector Int)
bitSplitWE splitPoint bits = 
    let (a,b) = Vector.splitAt splitPoint bits
    in if Vector.length a < splitPoint then Left "Not Enough bits!" else return (a,b) 

oneBitWE :: Vector Int -> Either Text (Int, Vector Int)
oneBitWE v | Vector.length v == 0 = Left "Not enough bits to decode one bit"
           | otherwise = 
               let (oneBit, rest) = Vector.splitAt 1 v
               in return (fromMaybe 0 (oneBit Vector.!? 0), rest)

getTenseGroups :: OneHotCaps -> Vector Int -> Int -> Vector TenseBitGroup 
                  -> Either Text (Vector TenseBitGroup, Vector Int)
getTenseGroups caps bits tenseId groups 
    | tenseId >= ohcTenses caps = return (reverse groups, bits)
    | otherwise = do
        let tenseLength = length $ encodeOneHotTense Nothing
        (tenseBits, rest) <- bitSplitWE (tenseLength * ohcTenses caps) bits
        (startBits, endBits) <- bitSplitWE tenseLength tenseBits
        getTenseGroups caps rest (tenseId+1) 
            $ Vector.cons (TenseBitGroup startBits endBits) groups

getWordGroups :: OneHotCaps -> Vector Int -> Int -> Vector WordBitGroup
                 -> Either Text (Vector WordBitGroup, Vector Int)
getWordGroups caps bits wordId groups
    | wordId >= ohcWords caps = return (reverse groups, bits)
    | otherwise = do
        let log2 r 1 = r
            log2 r n = log2 (r+1) (n `div` 2)
            log2Tenses = log2 0 $ ohcTenses caps
            log2Words = log2 0 $ ohcWords caps
            wordGroupSize = 4 + log2Tenses + log2Words
        (wordBits, rest) <- bitSplitWE wordGroupSize bits
        (present, wordRest) <- oneBitWE wordBits
        (subject, wordRest') <- oneBitWE wordRest
        (object, wordRest'') <- oneBitWE wordRest'
        (analogy, wordRest''') <- oneBitWE wordRest''
        (tenseId, referentId) <- bitSplitWE log2Tenses wordRest'''
        getWordGroups caps rest (wordId+1)
            $ Vector.cons (WordBitGroup wordId present subject object analogy tenseId referentId) groups
        

getBitGroups :: OneHotCaps -> Vector Int -> Either Text (Vector TenseBitGroup, Vector WordBitGroup)
getBitGroups caps bits = do
    (tenses, remainder) <- getTenseGroups caps bits 0 Vector.empty
    (wordGroups, _) <- getWordGroups caps remainder 0 Vector.empty
    return (tenses, wordGroups)

buildTenses :: Vector TenseBitGroup -> Vector Tense
buildTenses = Vector.map buildTense
    where buildTense (TenseBitGroup tenseStart tenseEnd) =
              (fromMaybe NTNow $ decodeOneHotTense tenseStart, fromMaybe NTNow $ decodeOneHotTense tenseEnd)

buildModifiers :: Vector WordBitGroup -> Vector (Modifier Int)
buildModifiers = Vector.map buildModifier
    where buildModifier (WordBitGroup wId _ _ _ _ _ rId) = 
              Modifier wId $ decodeBitVector rId

buildAnalogies :: Vector WordBitGroup -> Vector (Analogy Int)
buildAnalogies groups = Vector.map buildAnalogy $ Vector.filter isAnalogy groups
    where isAnalogy (WordBitGroup _ _ _ _ a _ _) = a == 1
          buildAnalogy (WordBitGroup wordId _ _ _ _ tenseBits objectBits) = 
              Analogy tenseId subjectId objectId wordId
                  where tenseId = decodeBitVector tenseBits
                        objectId = decodeBitVector objectBits
                        isObject (WordBitGroup wId _ _ _ _ _ _) = wId == objectId 
                        getReferent Nothing = 0
                        getReferent (Just (WordBitGroup _ _ _ _ _ _ rId)) = decodeBitVector rId
                        subjectId = getReferent $ Vector.find isObject groups

decodeOneHotOutput :: OneHotCaps -> Vector Int -> Either Text (Sentence Int)
decodeOneHotOutput caps bits = do
    (tenseGroups, wordGroups) <- getBitGroups caps bits
    let isValidTense (TenseBitGroup s e) = isJust (decodeOneHotTense s) && isJust (decodeOneHotTense e)
        tenses = buildTenses $ Vector.filter isValidTense tenseGroups
        isPresent (WordBitGroup _ p _ _ _ _ _) = p == 1
        modifiers = buildModifiers $ Vector.filter isPresent wordGroups
        analogies = buildAnalogies wordGroups
    return $ Sentence tenses modifiers analogies
    
decodeEncodeOneHotOutput :: OneHotCaps -> Sentence Int -> Either Text (Sentence Int, Bool)
decodeEncodeOneHotOutput caps s = do
    encoding <- encodeOneHotOutput caps s
    rs <- decodeOneHotOutput caps (Vector.fromList encoding)
    return (rs, rs == s)
    
-- ModifiedPhrase has list of modifiers, relative modifiees, modified word, and list of extra words for structural grammar
type BasicPhrase = ([Int],[Int],Int,[Int])
data Phrase = SubjectP BasicPhrase 
            | ConjunctionP BasicPhrase 
            | AnalogyObjectP Int BasicPhrase BasicPhrase
            | PrimaryAnalogyObjectP Int BasicPhrase BasicPhrase
data PhraseCode = SubjectCode Int BasicPhrase 
                | ConjunctionCode BasicPhrase 
                | AnalogyObjectCode Int Int BasicPhrase BasicPhrase Tense
type Linearizer a = PhraseCode -> BuilderContext (LinearPhrase a)
          
type SubjectPhrase = Phrase
type AnalogyObjectPhrase = Phrase
type ConjunctionPhrase = Phrase
data BasicSentence = BasicSentence SubjectPhrase [AnalogyObjectPhrase]
data PhraseSentence = SimpleSentence BasicSentence
                      | CompoundSentence BasicSentence ConjunctionPhrase BasicSentence

modifieeTemplate :: [Int] -> Int -> BuilderContext [Int]
modifieeTemplate [] _ = return []
modifieeTemplate (m:rest) o = do
    choice <- getRandomR (True,False)
    let choose True = o
        choose _ = m
    restChoices <- modifieeTemplate rest o
    return $ choose choice : restChoices

createPhrase :: Int -> BuilderContext BasicPhrase
createPhrase maxModifiers = do
    modifierCount <- getRandomR (0, maxModifiers)
    modifierIds <- replicateM modifierCount nextM
    objectId <- nextM
    let nextIds = drop 1 $ modifierIds ++ [objectId]
    modifiees <- modifieeTemplate nextIds objectId
    extras <- replicateM maxModifiers nextM
    return (modifierIds, modifiees, objectId, extras)

buildBasicSentence :: Int -> Int -> Int -> BuilderContext BasicSentence
buildBasicSentence tenseId maxModifiers count = do
    subjectPhrase <- createPhrase maxModifiers
    primaryAnalogyPhrase <- createPhrase maxModifiers
    primaryObjectPhrase <- createPhrase maxModifiers
    analogyPhrases <- replicateM (count-1) (createPhrase maxModifiers)
    objectPhrases <- replicateM (count-1) (createPhrase maxModifiers)
    let subject = SubjectP subjectPhrase
        primaryAnalogyObject = PrimaryAnalogyObjectP tenseId analogyPhrase objectPhrase
        analogyObjects = zipWith (AnalogyObjectP tenseId) analogyPhrases objectPhrases
    return $ BasicSentence subject (primaryAnalogyObject:analogyObjects)

buildSimpleSentence :: Int -> Int -> BuilderContext PhraseSentence
buildSimpleSentence maxModifiers count = do
    bs <- buildBasicSentence 0 maxModifiers count
    return $ SimpleSentence bs

buildCompoundSentence :: Int -> Int -> BuilderContext PhraseSentence
buildCompoundSentence maxModifiers count = do
    splitCount <- getRandomR (1,count-1)
    conjunctionId <- nextM
    conjunctionExtras <- replicateM 2 nextM
    let conjunctionPhrase = ConjunctionP ([],[],conjunctionId, conjunctionExtras)
    bs1 <- buildBasicSentence 0 maxModifiers splitCount
    bs2 <- buildBasicSentence 1 maxModifiers (count-splitCount)
    return $ CompoundSentence bs1 conjunctionPhrase bs2

generateTense :: BuilderContext Tense
generateTense = do
    t1 <- getRandomR (0::Int,3::Int)
    t2 <- getRandomR (0::Int,3::Int)
    let getTense 0 = NTPast
        getTense 1 = NTNow
        getTense 2 = NTFuture
        getTense _ = NTPlusDelta
    return (getTense t1, getTense t2)
          
linearizePhrases :: (Phrase -> BuilderContext (LinearPhrase a)) -> PhraseSentence -> BuilderContext (LinearPhrase a)
linearizePhrases linearizer (CompoundSentence s1 c s2) = do
    linearS1 <- linearizePhrases linearizer (SimpleSentence s1)
    linearC <- linearizer c
    linearS2 <- linearizePhrases linearizer (SimpleSentence s2)
    return $ linearS1 ++ linearC ++ linearS2
linearizePhrases linearizer (SimpleSentence (BasicSentence subject analogyObjects)) = do
    let count = length analogyObjects
    splitCount <- getRandomR (0, count)
    linearSubject <- linearizer subject
    linearAnalogyObjects <- mapM linearizer analogyObjects
    let preGroup = concat $ take splitCount linearAnalogyObjects
        postGroup = concat $ drop splitCount linearAnalogyObjects
    return $ preGroup ++ linearSubject ++ postGroup
       
buildLogicalSentence :: Tense -> Tense -> PhraseSentence -> Sentence Int
buildLogicalSentence t1 t2 (CompoundSentence s1 _ s2) = mergeLogicalSentence ls1 ls2
    where ls1 = buildLogicalSentence t1 t1 (SimpleSentence s1)
          ls2 = buildLogicalSentence t2 t2 (SimpleSentence s2)
          tenses = Vector.fromList [t1,t2]
          mergeLogicalSentence (Sentence _ m1s a1s) (Sentence _ m2s a2s) 
              = Sentence tenses (m1s Vector.++ m2s) (a1s Vector.++ a2s)
buildLogicalSentence t1 _ (SimpleSentence (BasicSentence subject analogyObjects)) = Sentence tenses modifiers analogies
    where tenses = Vector.singleton t1
          modifiers = Vector.fromList $ concatMap (newModifiers (subjectId subject)) (subject:analogyObjects)
          analogies = Vector.fromList $ concatMap (newAnalogies (subjectId subject)) analogyObjects
          subjectId (SubjectP (_,_,s,_)) = Just s
          subjectId _ = Nothing
          newModifiers _ (SubjectP (ms,mees,s,_)) = zipWith Modifier (s:ms) (s:mees)
          newModifiers s (PrimaryAnalogyObjectP t a o) = newModifiers s (AnalogyObjectP t a o)
          newModifiers (Just s) (AnalogyObjectP _ (ams,amees,a,_) (oms,omees,o,_)) =
              modifierMs ++ analogyMs
                  where modifierMs = zipWith Modifier (ams++oms) (amees++omees)
                        analogyMs = [Modifier o s, Modifier a o]
          newModifiers _ _ = []
          newAnalogies s (PrimaryAnalogyObjectP tId a o) = newAnalogies s (AnalogyObjectP tId a o)
          newAnalogies (Just s) (AnalogyObjectP tId (_,_,a,_) (_,_,o,_)) = [Analogy tId s o a]
          newAnalogies _ _ = []
       
packSentence :: LinearPhrase a -> Sentence Int -> Maybe (Sentence Int)
packSentence lps = mapMSentence packIds
    where wordIds = map fst lps
          wordDict = Map.fromList $ zip wordIds [0..]          
          packIds v = Map.lookup v wordDict 
       
getInputSentence :: LinearPhrase Text -> InputSentence
getInputSentence lps = Vector.fromList $ map InputText wordTexts
    where wordTexts = map snd lps
      
computeRelativeCode :: BasicPhrase -> Int
computeRelativeCode (_, mees, s, _) = meesCode
    where meesCode = decodeBitList $ map (\mee -> if mee == s then 0 else 1) mees
       
relativeLinearizer :: Linearizer a -> (Int -> Tense) -> Phrase -> BuilderContext (LinearPhrase a)
relativeLinearizer l _ (SubjectP p) = l (SubjectCode (computeRelativeCode p) p)
relativeLinearizer l _ (ConjunctionP p) = l (ConjunctionCode p)
relativeLinearizer l t (AnalogyObjectP tId p1 p2) = l (AnalogyObjectCode c1 c2 p1 p2 (t tId))
    where c1 = computeRelativeCode p1
          c2 = computeRelativeCode p2
       
generateSentenceM :: Linearizer a -> WordGenerator a -> Int -> BuilderContext (InputSentence, Sentence Int)
generateSentenceM linearizer wordGenerator maxModifiers = do
    tenseCount <- getRandomR (1,2)
    analogiesCount <- getRandomR (tenseCount, 10)
    tense1 <- generateTense
    tense2 <- generateTense
    let buildSentence 1 = buildSimpleSentence
        buildSentence _ = buildCompoundSentence
        getTense 0 = tense1
        getTense _ = tense2
        rl = relativeLinearizer linearizer getTense
    sentence <- buildSentence tenseCount maxModifiers analogiesCount
    linearSentence <- linearizePhrases rl sentence
    wordSentence <- wordGenerator linearSentence
    let logicalSentence = buildLogicalSentence tense1 tense2 sentence
        (Just packedLogicalSentence) = packSentence linearSentence logicalSentence
        inputSentence = getInputSentence wordSentence
    return (inputSentence, packedLogicalSentence)

generateSentence :: Linearizer a -> WordGenerator a -> Int -> StdGen -> (InputSentence, Sentence Int)
generateSentence linearizer wg maxModifiers g = evalState (evalRandT (generateSentenceM linearizer wg maxModifiers) g) 0
