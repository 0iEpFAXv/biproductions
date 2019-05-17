module BiProductionsLib
    ( testMessage,
      AbstractTime (NTMinusInfinity, NTMinus, NTMinusDelta, NTPast, NTNow, NTFuture, NTPlusDelta, NTPlus, NTPlusInfinity), 
      Tenses, InputWord(InputText, InputTime, InputRelativeTime), InputSentence,
      WordId, TenseId, Modifier(Modifier), Analogy(Analogy), Sentence(Sentence),
      OneHotCaps(ohcWords, ohcTenses), defaultOneHotCaps, wordOrdinals, padOrdinals,
      encodeBitList, decodeBitList, decodeBitVector,
      encodeOneHotInput, encodeOneHotOutput, encodeOneHotOutputWord, decodeOneHotOutput, getBitGroups,
      decodeOneHotTense, decodeEncodeOneHotOutput,
      TenseBitGroup(TenseBitGroup), WordBitGroup(WordBitGroup), buildTenses
    ) where

import ClassyPrelude
import qualified Data.Map.Strict as Map
import qualified Data.Vector as Vector
import qualified Data.Char as Char

testMessage :: Text
testMessage = "Hello World"

data AbstractTime = NTMinusInfinity | NTMinus | NTMinusDelta 
                  | NTPast | NTNow | NTFuture
                  | NTPlusDelta | NTPlus | NTPlusInfinity 
                  deriving (Eq, Read, Show)
type Tenses = Vector (AbstractTime, AbstractTime)
data InputWord = InputText Text | InputTime Text UTCTime | InputRelativeTime Text Double 
                 deriving (Eq, Read, Show)
type InputSentence = Vector InputWord

type WordId = Int
type TenseId = Int
data Modifier = Modifier WordId WordId deriving (Eq, Read, Show)
type Modifiers = Vector Modifier
data Analogy = Analogy TenseId WordId WordId WordId deriving (Eq, Read, Show)
type Analogies = Vector Analogy
data Sentence = Sentence Tenses Modifiers Analogies deriving (Eq, Read, Show)

data OneHotCaps = OneHotCaps { ohcWords :: Int, ohcTenses :: Int }
defaultOneHotCaps :: OneHotCaps
defaultOneHotCaps = OneHotCaps { ohcWords = 64, ohcTenses = 2}

type WordOrdinals = Text -> ([Int], [Int])

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

wordOrdinals :: [Text] -> WordOrdinals
wordOrdinals dictionary = lookUps
    where binaryValues = map encodeBitList [1..]
          indexedSet l = Map.fromAscList $ zip (sort l) binaryValues
          s = indexedSet (map toLower dictionary)
          rs = indexedSet (map (toLower . reverse) dictionary)
          lookUp :: Text -> Map Text [Int] -> [Int]
          lookUp w d = maybe [0] snd (Map.lookupLE w d)
          lookUps w = (lookUp lw s, lookUp (reverse lw) rs) where lw = toLower w

padOrdinal :: Int -> [Int] -> [Int]
padOrdinal size l | 2 ^ length l < size = padOrdinal size (0:l)
                  | otherwise = l

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
   

encodeOneHotOutput :: OneHotCaps -> Sentence -> Either Text [Int]
encodeOneHotOutput caps s = do
    tenses <- encodeOneHotTenses caps s 0
    encodedWords <- encodeOneHotOutputWords caps s 0
    return $ tenses ++ encodedWords
    
--   tensesCap blocks of:
--     7 bit: select AbstractTime start
--     7 bit: select AbstractTime end
encodeOneHotTenses :: OneHotCaps -> Sentence -> Int -> Either Text [Int]
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
encodeOneHotOutputWords :: OneHotCaps -> Sentence -> Int -> Either Text [Int]
encodeOneHotOutputWords caps s wordId
    | wordId >= ohcWords caps = return []
    | otherwise = do
        wordBlock <- encodeOneHotOutputWord caps s wordId
        rest <- encodeOneHotOutputWords caps s (wordId+1)
        return (wordBlock ++ rest)
        
encodeOneHotOutputWord :: OneHotCaps -> Sentence -> Int -> Either Text [Int]
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

buildTenses :: Vector TenseBitGroup -> Vector (AbstractTime, AbstractTime)
buildTenses = Vector.map buildTense
    where buildTense (TenseBitGroup tenseStart tenseEnd) =
              (fromMaybe NTNow $ decodeOneHotTense tenseStart, fromMaybe NTNow $ decodeOneHotTense tenseEnd)

buildModifiers :: Vector WordBitGroup -> Vector Modifier
buildModifiers = Vector.map buildModifier
    where buildModifier (WordBitGroup wId _ _ _ _ _ rId) = 
              Modifier wId $ decodeBitVector rId

buildAnalogies :: Vector WordBitGroup -> Vector Analogy
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

decodeOneHotOutput :: OneHotCaps -> Vector Int -> Either Text Sentence
decodeOneHotOutput caps bits = do
    (tenseGroups, wordGroups) <- getBitGroups caps bits
    let isValidTense (TenseBitGroup s e) = isJust (decodeOneHotTense s) && isJust (decodeOneHotTense e)
        tenses = buildTenses $ Vector.filter isValidTense tenseGroups
        isPresent (WordBitGroup _ p _ _ _ _ _) = p == 1
        modifiers = buildModifiers $ Vector.filter isPresent wordGroups
        analogies = buildAnalogies wordGroups
    return $ Sentence tenses modifiers analogies
    
decodeEncodeOneHotOutput :: OneHotCaps -> Sentence -> Either Text (Sentence, Bool)
decodeEncodeOneHotOutput caps s = do
    encoding <- encodeOneHotOutput caps s
    rs <- decodeOneHotOutput caps (Vector.fromList encoding)
    return (rs, rs == s)
    
    