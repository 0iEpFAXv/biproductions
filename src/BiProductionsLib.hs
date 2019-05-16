module BiProductionsLib
    ( testMessage,
      AbstractTime, Tenses, InputWord, InputSentence,
      WordId, TenseId, Object, AnalogyValue, Analogy, Sentence, RParagraph,
      OneHotCaps(ohcWords, ohcTenses), defaultOneHotCaps, wordOrdinals, padOrdinals,
      encodeOneHot
    ) where

import ClassyPrelude
import qualified Data.Map.Strict as Map
import qualified Data.Vector as Vector
import qualified Data.Char as Char
import Numeric (showIntAtBase)

testMessage :: Text
testMessage = "Hello World"

data AbstractTime = NTMinusInfinity | NTMinus | NTMinusDelta 
                  | NTRelative Double | NTNow | NTDate UTCTime 
                  | NTPlusDelta | NTPlus | NTPlusInfinity 
                  deriving (Eq, Read, Show)
type Tenses = Vector (AbstractTime, AbstractTime)
data InputWord = InputText Text | InputTime Text UTCTime | InputRelativeTime Text Double 
                 deriving (Eq, Read, Show)
type InputSentence = Vector InputWord

type WordId = Int
type TenseId = Int
data Object = Object WordId | ModifiedObject WordId Object deriving (Eq, Read, Show)
type Objects = Vector Object
data AnalogyValue = AnalogyValue WordId | ModifiedAnalogyValue WordId AnalogyValue 
                    deriving (Eq, Read, Show)
data Analogy = Analogy TenseId WordId WordId AnalogyValue deriving (Eq, Read, Show)
type Analogies = Vector Analogy
data Sentence = Sentence InputSentence Tenses Objects Analogies deriving (Eq, Read, Show)
type RParagraph = [Sentence] 

data OneHotCaps = OneHotCaps { ohcWords :: Int, ohcTenses :: Int }
defaultOneHotCaps :: OneHotCaps
defaultOneHotCaps = OneHotCaps { ohcWords = 64, ohcTenses = 2}

encodeOneHot :: [Text] -> OneHotCaps -> Sentence -> Either Text ([Int], [Int])
encodeOneHot dictionary caps s = do
    input <- encodeOneHotInput dictionary caps s
    output <- encodeOneHotOutput caps s
    return (input, output)

type WordOrdinals = Text -> ([Int], [Int])

binaryValue :: Int -> [Int]
binaryValue n = map charToInteger (showIntAtBase 2 intToChar n "")
    where charToInteger '0' = 0
          charToInteger _ = 1
          intToChar 0 = '0'
          intToChar _ = '1'

wordOrdinals :: [Text] -> WordOrdinals
wordOrdinals dictionary = lookUps
    where binaryValues = map binaryValue [1..]
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

encodeOneHotInput :: [Text] -> OneHotCaps -> Sentence -> Either Text [Int]
encodeOneHotInput dictionary = encodeOneHotInputWords (padOrdinals 16 8 (wordOrdinals dictionary)) 0

encodeOneHotInputWords :: WordOrdinals -> Int -> OneHotCaps -> Sentence -> Either Text [Int]
encodeOneHotInputWords ordinals wordId caps s 
    | wordId >= ohcWords caps = return []
    | otherwise = do
        let word = getWordFromSentence s wordId
            paddedWord = padOrdinal (ohcWords caps) $ binaryValue wordId
        wordBlock <- encodeOneHotInputWord ordinals word paddedWord
        rest <- encodeOneHotInputWords ordinals (wordId+1) caps s
        return (wordBlock ++ rest)

getWordText :: Maybe InputWord -> Text
getWordText Nothing = ""
getWordText (Just (InputText t)) = t
getWordText (Just (InputTime t _)) = t
getWordText (Just (InputRelativeTime t _)) = t

getWordFromSentence :: Sentence -> Int -> Maybe InputWord
getWordFromSentence (Sentence is _ _ _) wordId = is Vector.!? wordId

-- Input: (29 + log_2 wordCap)*wordCap = 2176
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
   

-- Output: tensesCap*19 + wordCap*(5 + log_2 tensesCap + log_2 wordCap) = 
encodeOneHotOutput :: OneHotCaps -> Sentence -> Either Text [Int]
encodeOneHotOutput caps s = do
    tenses <- encodeOneHotTenses caps s 0
    encodedWords <- encodeOneHotOutputWords caps s 0
    return $ tenses ++ encodedWords
    
--   tensesCap blocks of:
--     9 bit: select AbstractTime start
--     9 bit: select AbstractTime end
encodeOneHotTenses :: OneHotCaps -> Sentence -> Int -> Either Text [Int]
encodeOneHotTenses caps s tenseId
    | tenseId >= ohcTenses caps = return []
    | otherwise = do
        let getSentenceTense (Sentence _ tenses _ _) = tenses Vector.!? tenseId
            propMaybe (Just (t1, t2)) = (Just t1, Just t2)
            propMaybe _ = (Nothing, Nothing)
            (mTenseStart, mTenseEnd) = propMaybe $ getSentenceTense s
            tenseStart = encodeOneHotTense mTenseStart
            tenseEnd = encodeOneHotTense mTenseEnd
        rest <- encodeOneHotTenses caps s (tenseId+1)
        return $ tenseStart ++ tenseEnd ++ rest
    
encodeOneHotTense :: Maybe AbstractTime -> [Int]
encodeOneHotTense Nothing = [0,0,0,0,0,0,0,0,0]
encodeOneHotTense (Just NTMinusInfinity) = [1,0,0,0,0,0,0,0,0]
encodeOneHotTense (Just NTMinus) = [0,1,0,0,0,0,0,0,0]
encodeOneHotTense (Just NTMinusDelta) = [0,0,1,0,0,0,0,0,0]
encodeOneHotTense (Just (NTRelative _)) = [0,0,0,1,0,0,0,0,0]
encodeOneHotTense (Just NTNow) = [0,0,0,0,1,0,0,0,0]
encodeOneHotTense (Just (NTDate _)) = [0,0,0,0,0,1,0,0,0]
encodeOneHotTense (Just NTPlusDelta) = [0,0,0,0,0,0,1,0,0]
encodeOneHotTense (Just NTPlus) = [0,0,0,0,0,0,0,1,0]
encodeOneHotTense (Just NTPlusInfinity) = [0,0,0,0,0,0,0,0,1]

--NTMinusInfinity | NTMinus | NTMinusDelta 
--                  | NTRelative Double | NTNow | NTDate UTCTime 
--                  | NTPlusDelta | NTPlus | NTPlusInfinity 

--   wordCap blocks of:
--     1 bit: is present
--     1 bit: is modifier
--     1 bit: is subject
--     1 bit: is object
--     1 bit: is analogy
--     log_2 tensesCap bits: select tense
--     log_2 wordCap bits: referent word id
--       Notes: For modifier, referent word id refers to object or analogy that is being modified
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
encodeOneHotOutputWord caps (Sentence sentence _ objects analogies) wordId = 
    let present = if wordId >= length sentence then 0 else 1
        
        isObjectModifier (Object _) = False
        isObjectModifier (ModifiedObject m o) 
            | m == wordId = True
            | otherwise = isObjectModifier o
        hasObjectModifier = Vector.any isObjectModifier objects
        isAnalogyValueModifier (AnalogyValue _) = False
        isAnalogyValueModifier (ModifiedAnalogyValue m a) 
            | m == wordId = True
            | otherwise = isAnalogyValueModifier a                                                           
        isAnalogyModifier (Analogy _ _ _ analogyValue) = isAnalogyValueModifier analogyValue
        hasAnalogyModifier = Vector.any isAnalogyModifier analogies
        hasModifier = hasObjectModifier || hasAnalogyModifier
        modifier = if hasModifier then 1 else 0
        
        isSubject (Analogy _ s _ _) = s == wordId
        isAnySubject = Vector.any isSubject analogies
        subject = if isAnySubject then 1 else 0
        
        isObject (Analogy _ _ o _) = o == wordId
        isAnyObject = Vector.any isObject analogies
        object = if isAnyObject then 1 else 0

        isAnalogyValue (AnalogyValue analogyValue) = analogyValue == wordId
        isAnalogyValue (ModifiedAnalogyValue _ a) = isAnalogyValue a
        isAnalogy (Analogy _ _ _ analogyValue) = isAnalogyValue analogyValue
        isAnyAnalogy = Vector.any isAnalogy analogies
        analogy = if isAnyAnalogy then 1 else 0
        
        findTense = maybe 0 (\(Analogy tId _ _ _) -> tId) $ Vector.find isAnalogy analogies
        tenseId = if isAnyAnalogy then findTense else 0
        tense = padOrdinal (ohcTenses caps) (binaryValue tenseId)
        
        getObject Nothing = Nothing
        getObject (Just (ModifiedObject _ o)) = getObject (Just o)
        getObject (Just (Object o)) = Just o
        findReferentOfObjectModifier = getObject $ Vector.find isObjectModifier objects
        getAnalogyValue (AnalogyValue a) = a
        getAnalogyValue (ModifiedAnalogyValue _ a) = getAnalogyValue a
        getAnalogy Nothing = Nothing
        getAnalogy (Just (Analogy _ _ _ analogyValue)) = Just $ getAnalogyValue analogyValue
        findReferentOfAnalogyModifier = getAnalogy $ Vector.find isAnalogyModifier analogies
        findReferentOfSubject = if isAnySubject then Just wordId else Nothing
        getSubject Nothing = Nothing
        getSubject (Just (Analogy _ s _ _)) = Just s
        findReferentOfObject = getSubject $ Vector.find isObject analogies
        getAnalogyObject Nothing = Nothing
        getAnalogyObject (Just (Analogy _ _ o _)) = Just o
        findReferentOfAnalogy = getAnalogyObject $ Vector.find isAnalogy analogies
        finders = [findReferentOfObjectModifier, 
                   findReferentOfAnalogyModifier,
                   findReferentOfSubject,
                   findReferentOfObject,
                   findReferentOfAnalogy]
        findReferentId = listToMaybe $ catMaybes finders
        referentId = fromMaybe 0 findReferentId
        referent = padOrdinal (ohcWords caps) (binaryValue referentId)
    in return $ [present, modifier, subject, object, analogy] ++ tense ++ referent

        