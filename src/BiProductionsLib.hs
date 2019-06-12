module BiProductionsLib
    ( InputWord(InputText, InputTime, InputRelativeTime), InputSentence,
      TenseId, Modifier(Modifier), Analogy(Analogy), Sentence(Sentence),
      OneHotCaps(ohcWords, ohcTenses), ohcSplit, ohcTensesWidth, defaultOneHotCaps, wordOrdinals, WordOrdinals,
      mapMSentence,
      encodeInputWords, encodeInputWord, decodeInputWord, encodeOutputWord, encodeOutputIsAnalogy, encodeTenses, 
      decodeOneHotTense, decodeEncodeOneHotOutput, decodeSentence, decodeTenses,
      Phrase(SubjectP, ConjunctionP, AnalogyObjectP, PrimaryAnalogyObjectP),
      generateSentence, 
      
      -- From EnglishExamples
      BuilderContext, nextM,
      AbstractTime (NTMinusInfinity, NTMinus, NTMinusDelta, NTPast, NTNow, NTFuture, NTPlusDelta, NTPlus, NTPlusInfinity), 
      Tense, Tenses,
      LinearPhrase, WordGenerator, 
      BasicPhrase, PhraseCode(SubjectCode, ConjunctionCode, PrimaryAnalogyCode, AnalogyCode, ObjectCode), Linearizer, 
      encodeBitList, decodeBitList, decodeBitVector, padOrdinal,
      basicExampleData, exampleDataWithDescriptor, nounExampleInfo, verbExampleInfo, preVerbExampleInfo,
      showPossibleWords, showExamples, readFileExamples, 
      writeFileExamples, readWordGenerator, readLinearizer      
      
    ) where

import ClassyPrelude
import qualified Data.Map.Strict as Map
import qualified Data.Vector as Vector
import qualified Data.Char as Char
import Data.Sort (uniqueSort)
import Control.Monad.Random.Lazy hiding (replicateM)
import Control.Monad.Trans.State.Lazy
import EnglishExamples
import Data.Bits (shift, (.&.), Bits)

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

ohcSplit :: OneHotCaps -> Int
ohcSplit caps = ohcWords caps `div` 2

type WordOrdinals a = Text -> (a, a)

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
          
wordOrdinals :: (Integral a, Bits a) => [Text] -> WordOrdinals a
wordOrdinals dictionary = lookUps
    where count = length dictionary
          findMaxBit :: (Integral a, Bits a) => a -> Int -> Int
          findMaxBit 0 mB = mB
          findMaxBit n mB = findMaxBit (shift n (-1)) (mB+1)
          maxBit = findMaxBit count 0
          indexedSet l removeBits = Map.fromAscList $ zip (uniqueSort l) (map (`shift` removeBits) [1..])
          s = indexedSet (map toLower dictionary) (min 0 (16 - maxBit)) 
          rs = indexedSet (map (toLower . reverse) dictionary) (min 0 (8 - maxBit)) 
          lookUp :: Integral a => Text -> Map Text a -> a
          lookUp w d = maybe 0 snd (Map.lookupLE w d)
          lookUps w = (lookUp lw s, lookUp (reverse lw) rs) where lw = toLower w

-- wordCap integers made of:
--   1 bit: is present
--   1 bit: is capitalized
--   16 bits: most significant bits of word ordinal sorted forward
--   8 bits: most significant bits of word ordinal sorted reverse
encodeInputWords :: Int -> WordOrdinals Int -> OneHotCaps -> InputSentence -> Either Text [Int]
encodeInputWords pivot ordinals caps s | pivot >= length s = Left "pivot word is past end of sentence"
                                       | otherwise = Right $ fWords' ++ [wordNum] ++ bWords'
    where (f, r) = splitAt (pivot-1) $ Vector.toList s
          mR = uncons r
          (w, b) = fromMaybe (InputText "", []) mR
          encodeW = encodeInputWord ordinals
          padWords ws = take (ohcSplit caps) $ map encodeW ws ++ repeat 0
          fWords' = reverse $ padWords (reverse f)
          wordNum = encodeW w
          bWords' = padWords b

getWordText :: Maybe InputWord -> Text
getWordText Nothing = ""
getWordText (Just (InputText t)) = t
getWordText (Just (InputTime t _)) = t
getWordText (Just (InputRelativeTime t _)) = t

encodeInputWord :: WordOrdinals Int -> InputWord -> Int
encodeInputWord ordinals iw = wordInt
    where wordText = getWordText $ Just iw
          present = if wordText == "" then 0 else 1
          capitalB = maybe False (Char.isUpper . fst) $ uncons wordText
          capital = if capitalB then 1 else 0
          (ordF, ordR) = ordinals wordText
          wordInt = shift present (1+16+8) + shift capital (16+8) + shift ordF 8 + ordR 

decodeInputWord :: WordOrdinals Integer -> InputWord -> Integer -> Either Text InputWord
decodeInputWord ordinals iw wordInt | present' /= present = Left "present bits do not match"
                                    | capital' /= capital = Left "capital bits do not match"
                                    | ordF' /= ordF = Left "ordF number does not match"
                                    | ordR' /= ordR = Left "ordR number does not match"
                                    | wordInt' /= wordInt = Left "wordInt does not match"
                                    | otherwise = Right $ InputText wordText'
    where wordText' = getWordText $ Just iw
          present' = if wordText' == "" then 0 else 1
          capitalB' = maybe False (Char.isUpper . fst) $ uncons wordText'
          capital' = if capitalB' then 1 else 0
          (ordF', ordR') = ordinals wordText'
          wordInt' = shift present' (1+16+8) + shift capital' (16+8) + shift ordF' 8 + ordR' 
          ordR = wordInt .&. ((2::Integer)^(8::Integer)-1)
          ordF = shift wordInt (-8) .&. ((2::Integer)^(16::Integer)-1)
          capital = shift wordInt (-8-16) .&. 1::Integer
          present = shift wordInt (-8-16-1) .&. 1::Integer


--   wordCap block of:
--     --1 bit: is subject
--     --1 bit: is object
--     --1 bit: is analogy
--     wordCap bits: referent word id
--       Notes: For modifier, referent word id refers to modifier, object or analogy that is being modified
--              For subject, referent word id refers to subject
--              For object, referent word id refers to subject
--              For analogy, referent word id refers to object
ohcWordWidth :: OneHotCaps -> Int
ohcWordWidth caps = 1 + ohcWords caps

encodeOutputWord :: Int -> OneHotCaps -> Sentence Int -> Either Text Int
encodeOutputWord wordId caps (Sentence _ modifiers _) | not present = Right 0
                                                      | otherwise = Right wordInt
    where present = Vector.any isModifier modifiers
    
          getReferent Nothing = wordId 
          getReferent (Just (Modifier _ r)) = r
          isModifier (Modifier m _) = m == wordId
          referentId = getReferent $ Vector.find isModifier modifiers
          relativeRId = max 1 . min (2+ohcWords caps) $ 1 + referentId - wordId + 1+ohcSplit caps

          wordInt = relativeRId
                    
ohcAnalogyWidth :: OneHotCaps -> Int
ohcAnalogyWidth _ = 3

encodeOutputIsAnalogy :: Int -> OneHotCaps -> Sentence Int -> Either Text Int
encodeOutputIsAnalogy wordId _ (Sentence _ modifiers analogies) | not present = Right 0
                                                                   | otherwise = Right analogy
    where present = Vector.any isModifier modifiers
          isModifier (Modifier m _) = m == wordId
    
          isAnalogy (Analogy _ _ _ a) = a == wordId
          isAnyAnalogy = Vector.any isAnalogy analogies
          analogy = if isAnyAnalogy then 2 else 1
                            
--   tensesCap blocks of:
--     7 bit: select AbstractTime start
--     7 bit: select AbstractTime end
encodeTenses :: OneHotCaps -> Sentence Int -> Int -> Either Text Integer
encodeTenses caps s tenseId
    | tenseId >= ohcTenses caps = return 0
    | otherwise = do
        let getSentenceTense (Sentence tenses _ _) = tenses Vector.!? tenseId
            propMaybe (Just (t1, t2)) = (Just t1, Just t2)
            propMaybe _ = (Nothing, Nothing)
            (mTenseStart, mTenseEnd) = propMaybe $ getSentenceTense s
            tenseStart = encodeTense mTenseStart
            tenseEnd = encodeTense mTenseEnd
        rest <- encodeTenses caps s (tenseId+1)
        return $ rest * (2::Integer)^(9+9::Integer) + tenseStart * (2::Integer)^(9::Integer) + tenseEnd
    
encodeTense :: Maybe AbstractTime -> Integer
encodeTense Nothing =                0
encodeTense (Just NTMinusInfinity) = 256
encodeTense (Just NTMinus) =         128
encodeTense (Just NTMinusDelta) =    64
encodeTense (Just NTPast) =          32
encodeTense (Just NTNow) =           16
encodeTense (Just NTFuture) =        8
encodeTense (Just NTPlusDelta) =     4
encodeTense (Just NTPlus) =          2
encodeTense (Just NTPlusInfinity) =  1

ohcTensesWidth :: OneHotCaps -> Int
ohcTensesWidth caps = ohcTenses caps * 2 * length (encodeOneHotTense Nothing)

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

decodeSentence :: OneHotCaps -> [Int] -> [[Int]] -> [[Int]] -> Either Text (Sentence Int)
decodeSentence caps ohTenses ohWs ohIsAnalogies = eSentence mTenses mModifiers mAnalogies
    where mTenses = decodeTenses caps (Vector.fromList ohTenses)
          mModifiers = decodeModifiers caps ohWs
          mAnalogies = decodeAnalogies caps mModifiers ohIsAnalogies
          eSentence (Just ts) (Just ms) (Just as) = Right (Sentence ts ms as)
          eSentence Nothing _ _ = Left "Could not decode tenses"
          eSentence (Just _) Nothing _ = Left "Could not decode modifiers"
          eSentence (Just _) (Just _) Nothing = Left "Could not decode analogies" 

decodeTenses :: OneHotCaps -> Vector Int -> Maybe Tenses
decodeTenses _ bits = mTenses mStartFirst mEndFirst mStartSecond mEndSecond
    where mStartSecond = decodeOneHotTense . Vector.take 9 $ bits
          mEndSecond = decodeOneHotTense . Vector.take 9 . Vector.drop 9 $ bits
          mStartFirst = decodeOneHotTense . Vector.take 9 . Vector.drop 18 $ bits
          mEndFirst = decodeOneHotTense . Vector.take 9 . Vector.drop 27 $ bits
          mTenses (Just s) (Just e) Nothing Nothing = Just $ Vector.singleton (s,e)
          mTenses (Just s1) (Just e1) (Just s2) (Just e2) = Just $ Vector.fromList [(s1,e1),(s2,e2)]
          mTenses _ _ _ _ = Nothing

decodeOneHotTense :: Vector Int -> Maybe AbstractTime
decodeOneHotTense bits = decodeTenseId $ Vector.findIndex (== 1) (reverse bits)
decodeTenseId :: Maybe Int -> Maybe AbstractTime
decodeTenseId (Just 8) = Just NTMinusInfinity
decodeTenseId (Just 7) = Just NTMinus
decodeTenseId (Just 6) = Just NTMinusDelta
decodeTenseId (Just 5) = Just NTPast
decodeTenseId (Just 4) = Just NTNow
decodeTenseId (Just 3) = Just NTFuture
decodeTenseId (Just 2) = Just NTPlusDelta
decodeTenseId (Just 1) = Just NTPlus
decodeTenseId (Just 0) = Just NTPlusInfinity
decodeTenseId _ =        Nothing

decodeModifiers :: OneHotCaps -> [[Int]] -> Maybe (Modifiers Int)
decodeModifiers caps ohWs = Just modifiers
    where ws = map decodeBitList ohWs
          modifiers = Vector.fromList $ mapMaybe (decodeModifier (ohcSplit caps)) (zip [0..] ws)

decodeModifier :: Int -> (Int, Int) -> Maybe (Modifier Int)
decodeModifier splitCount (wordId, rCode) | rCode == 0 = Nothing
                                          | otherwise = Just $ Modifier wordId referentId
    where referentId = rCode + wordId - 1 - splitCount - 1


getReferentId :: Maybe Int -> Modifiers Int -> Maybe Int
getReferentId Nothing _ = Nothing
getReferentId (Just mId) ms = (\(Modifier _ rId) -> Just rId) =<< Vector.find isModifier ms
    where isModifier (Modifier m _) = m == mId

getModifierIds :: Maybe Int -> Modifiers Int -> [Int]
getModifierIds Nothing _ = []
getModifierIds (Just rId) ms = map (\(Modifier mId _) -> mId) $ filter isReferent (Vector.toList ms)
    where isReferent (Modifier _ r) = r == rId

decodeAnalogies :: OneHotCaps -> Maybe (Modifiers Int) -> [[Int]] -> Maybe (Analogies Int)
decodeAnalogies _ Nothing _ = Nothing
decodeAnalogies _ (Just ms) ohAs = Just analogies
    where analogies = Vector.fromList $ mapMaybe (decodeAnalogy subjects ms) (zip [0..] ohAs)
          subjects = map (\(Modifier _ sId) -> sId) . filter hasReferrer . filter isSelfReferent $ Vector.toList ms
              where isSelfReferent (Modifier mId rId) = mId == rId
                    hasReferrer (Modifier _ rId) = not . null  . getModifierIds (Just rId) . filter (not . isSelfReferent) $ ms

decodeAnalogy :: [Int] -> Modifiers Int -> (Int, [Int]) -> Maybe (Analogy Int)
decodeAnalogy [] _ _ = Nothing
decodeAnalogy subjects ms (wordId, ohA) = mAnalogy mSubject mO mA
    where idSubjects :: [(Int,Int)]
          idSubjects = zip [0..] subjects
          mA = if decodeBitList ohA == 2 then Just wordId else Nothing
          mO = getReferentId (Just wordId) ms
          mRef2 = getReferentId mO ms
          mSubject :: Maybe (Int,Int)
          mSubject = find (\(_,s) -> Just s == mRef2) idSubjects
          mAnalogy :: Maybe (Int,Int) -> Maybe Int -> Maybe Int -> Maybe (Analogy Int)
          mAnalogy (Just (tId,s)) (Just o) (Just a) = Just $ Analogy tId s o a
          mAnalogy _ _ _ = Nothing
          
data WordBitGroup = WordBitGroup Int Int Int Int Int (Vector Int) (Vector Int) deriving (Eq, Show, Read)
data TenseBitGroup = TenseBitGroup (Vector Int) (Vector Int) deriving (Eq, Show, Read)

decodeEncodeOneHotOutput :: OneHotCaps -> Sentence Int -> Int -> Either Text (Sentence Int, Bool)
decodeEncodeOneHotOutput caps s wordCount = do
    encodings <- mapM (\wId -> encodeOutputWord wId caps s) [0..(wordCount-1)]
    isAnalogies <- mapM (\wId -> encodeOutputIsAnalogy wId caps s) [0..(wordCount-1)]
    tenses <- encodeTenses caps s 0   
    let ohEncodings = map (padOrdinal (ohcWordWidth caps) . encodeBitList) encodings
        ohIsAnalogies = map (padOrdinal (ohcAnalogyWidth caps) . encodeBitList) isAnalogies
        ohTenses = padOrdinal (ohcTensesWidth caps) . encodeBitList $ tenses
    rs <- decodeSentence caps ohTenses ohEncodings ohIsAnalogies
    return (rs, rs == s)
    
-- BasicPhrase has list of modifiers, modifiees, modified word, and list of extra words for structural grammar
data Phrase = SubjectP BasicPhrase 
            | ConjunctionP BasicPhrase 
            | AnalogyObjectP Int BasicPhrase BasicPhrase
            | PrimaryAnalogyObjectP Int BasicPhrase BasicPhrase
          
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
    -- This chain of modifer Counts helps reduce the probability of really long modifier chains.
    modifierCount'' <- getRandomR (0, maxModifiers)
    modifierCount' <- getRandomR (0, modifierCount'')
    modifierCount <- getRandomR (0, modifierCount')
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
        primaryAnalogyObject = PrimaryAnalogyObjectP tenseId primaryAnalogyPhrase primaryObjectPhrase
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
    t1 <- getRandomR (0::Int,2::Int)
    t2 <- getRandomR (t1,2::Int)
    let getTense 0 = NTPast
        getTense 1 = NTNow
        getTense 2 = NTFuture
        getTense _ = NTNow
    return (getTense t1, getTense t2)
          
linearizePhrases :: (Phrase -> BuilderContext (LinearPhrase Text)) -> PhraseSentence -> BuilderContext (LinearPhrase Text)
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
    
codeWidth :: BasicPhrase -> Int
codeWidth (ms, _, _, _) = length ms
       
computeAnalogyPhraseValues :: BasicPhrase -> BasicPhrase -> (Int, Int, Int, Int)
computeAnalogyPhraseValues p1 p2 = (c1, c2, w1, w2) 
    where c1 = computeRelativeCode p1
          c2 = computeRelativeCode p2
          w1 = codeWidth p1
          w2 = codeWidth p2
       
relativeLinearizer :: Linearizer Text -> (Int -> Tense) -> Phrase -> BuilderContext (LinearPhrase Text)
relativeLinearizer l _ (SubjectP p) = l (SubjectCode (codeWidth p) (computeRelativeCode p) p)
relativeLinearizer l _ (ConjunctionP p) = l (ConjunctionCode p)
relativeLinearizer l _ (AnalogyObjectP _ p1 p2) = do
    let (c1, c2, w1, w2) = computeAnalogyPhraseValues p1 p2
    aPhrase <- l (AnalogyCode w1 c1 p1)
    oPhrase <- l (ObjectCode w2 c2 p2)
    return $ aPhrase ++ oPhrase
relativeLinearizer l t (PrimaryAnalogyObjectP tId p1 p2) = do
    let (c1, c2, w1, w2) = computeAnalogyPhraseValues p1 p2
    aPhrase <- l (PrimaryAnalogyCode (t tId) w1 c1 p1)
    oPhrase <- l (ObjectCode w2 c2 p2)
    return $ aPhrase ++ oPhrase
       
generateSentenceM :: Linearizer Text -> WordGenerator Text -> Int -> BuilderContext (InputSentence, Sentence Int)
generateSentenceM linearizer wordGenerator maxModifiers = do
    tenseCount <- getRandomR (1,2)
    analogiesCount <- getRandomR (tenseCount, 6)
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

generateSentence :: Linearizer Text -> WordGenerator Text -> Int -> StdGen -> (InputSentence, Sentence Int)
generateSentence linearizer wg maxModifiers g = evalState (evalRandT (generateSentenceM linearizer wg maxModifiers) g) 0
