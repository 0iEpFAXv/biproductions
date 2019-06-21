module BiProductionsLib
    ( InputWord(InputText, InputTime, InputRelativeTime), InputSentence,
      TenseId, Modifier(Modifier), Analogy(Analogy), Sentence(Sentence),
      OneHotCaps(ohcWords, ohcTenses), ohcSplit, ohcTenseWidth, defaultOneHotCaps, wordOrdinals, WordOrdinals,
      mapMSentence,
      encodeInputWords, encodeSentenceInputWords, encodeSOVInputWords, encodeInputWord, 
      decodeInputWord, encodeOutputModifier, encodeOutputIs, encodeTense, 
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
      maybeReadUtf8, showPossibleWords, showExamples, readFileExamples, 
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
import Data.List (nub)
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

encodeInputWords :: Int -> WordOrdinals Int -> OneHotCaps -> InputSentence 
                        -> Either Text [Int]
encodeInputWords = encodeCoreInputWords encodeInputWord (16+8+1+1)

encodeCoreInputWords :: (WordOrdinals Int -> (InputWord, Int) -> Int) 
                        -> Int -> Int -> WordOrdinals Int -> OneHotCaps -> InputSentence 
                        -> Either Text [Int]
encodeCoreInputWords encoder padding pivot ordinals caps s | pivot >= length s = Left "pivot word is past end of sentence"
                                                           | otherwise = Right wordBits
    where (f, r) = splitAt (pivot-1) $ zip (Vector.toList s) [0..]
          mR = uncons r
          (w, b) = fromMaybe ((InputText "",pivot), []) mR
          encodeW = encoder ordinals
          padWords ws = take (ohcSplit caps) $ map encodeW ws ++ repeat 0
          fWords' = reverse $ padWords (reverse f)
          wordNum = encodeW w
          bWords' = padWords b
          wordBits = concatMap (padOrdinal padding . encodeBitList) $ fWords' ++ [wordNum] ++ bWords'

encodeSentenceInputWords :: Sentence Int -> Int -> WordOrdinals Int -> OneHotCaps -> InputSentence -> Either Text [Int]
encodeSentenceInputWords s = encodeCoreInputWords (encodeSentenceInputWord s) (1+1+16+8+1+1+1)

getWordText :: Maybe InputWord -> Text
getWordText Nothing = ""
getWordText (Just (InputText t)) = t
getWordText (Just (InputTime t _)) = t
getWordText (Just (InputRelativeTime t _)) = t

-- integers made of:
--   1 bit: is present
--   1 bit: is capitalized
--   16 bits: most significant bits of word ordinal sorted forward
--   8 bits: most significant bits of word ordinal sorted reverse
encodeInputWord :: WordOrdinals Int -> (InputWord, Int) -> Int
encodeInputWord ordinals (iw,_) = wordInt
    where wordText = getWordText $ Just iw
          present = if wordText == "" then 0 else 1
          capitalB = maybe False (Char.isUpper . fst) $ uncons wordText
          capital = if capitalB then 1 else 0
          (ordF, ordR) = ordinals wordText
          wordInt = shift present (1+16+8) + shift capital (16+8) + shift ordF 8 + ordR 

encodeSentenceInputWord :: Sentence Int -> WordOrdinals Int -> (InputWord, Int) -> Int
encodeSentenceInputWord (Sentence _ _ as) ordinals (iw, iwId) = encodeSOVInputWord subject object verb ordinals (iw, iwId)
    where isSubject (Analogy _ s _ _) = s == iwId
          isObject (Analogy _ _ o _) = o == iwId
          isVerb (Analogy _ _ _ v) = v == iwId
          subject = if Vector.any isSubject as then 1 else 0
          object = if Vector.any isObject as then 1 else 0
          verb = if Vector.any isVerb as then 1 else 0

encodeSOVInputWords :: [Int] -> [Int] -> [Int] -> Int -> WordOrdinals Int -> OneHotCaps -> InputSentence -> Either Text [Int]
encodeSOVInputWords ss os vs = encodeCoreInputWords (encodeSOVsInputWord ss os vs) (1+1+16+8+1+1+1)

-- integers made of:
--   1 bit: is present
--   1 bit: is capitalized
--   16 bits: most significant bits of word ordinal sorted forward
--   8 bits: most significant bits of word ordinal sorted reverse
--   1 bit: is subject
--   1 bit: is object
--   1 bit: is verb
encodeSOVsInputWord :: [Int] -> [Int] -> [Int] -> WordOrdinals Int -> (InputWord, Int) -> Int
encodeSOVsInputWord subjects objects verbs ordinals (iw,iwId) = encodeSOVInputWord subject object verb ordinals (iw,iwId)
    where subject = maybe 0 fst $ uncons (drop iwId subjects)
          object = maybe 0 fst $ uncons (drop iwId objects)
          verb = maybe 0 fst $ uncons (drop iwId verbs)

-- integers made of:
--   1 bit: is present
--   1 bit: is capitalized
--   16 bits: most significant bits of word ordinal sorted forward
--   8 bits: most significant bits of word ordinal sorted reverse
--   1 bit: is subject
--   1 bit: is object
--   1 bit: is verb
encodeSOVInputWord :: Int -> Int -> Int -> WordOrdinals Int -> (InputWord, Int) -> Int
encodeSOVInputWord subject object verb ordinals (iw,iwId) = wordInt'
    where wordInt = encodeInputWord ordinals (iw,iwId)
          wordInt' = shift wordInt 3 + shift subject 2 + shift object 1 + verb 

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

relativeRId :: OneHotCaps -> Int -> Int -> Int
relativeRId caps referentId wordId = max 1 . min (1+ohcWords caps) $ 1 + referentId - wordId + 1+ohcSplit caps

encodeOutputModifier :: Int -> OneHotCaps -> Sentence Int -> Either Text Int
encodeOutputModifier wordId caps (Sentence _ modifiers as) | not present || isJust analogyId = Right 0
                                                       | otherwise = Right wordInt
    where present = Vector.any isModifier modifiers
    
          isSOV (Analogy _ s o v) = s == wordId || o == wordId || v == wordId
          analogyId = Vector.find isSOV as
    
          getReferent Nothing = wordId 
          getReferent (Just (Modifier _ r)) = r
          isModifier (Modifier m _) = m == wordId
          referentId = getReferent $ Vector.find isModifier modifiers

          wordInt = relativeRId caps referentId wordId
                    
-- First argument of encodeOutputIs controls what category the word belongs to:
-- True False True = if verb then relative location of subject else 0
-- False True True = if verb then relative location of object else 0
-- True _ False = if subject then 1 else 0
-- _ True False = if object then 1 else 0
-- _ _ True = if verb then 1 else 0
encodeOutputIs :: Analogy Bool -> Int -> OneHotCaps -> Sentence Int -> Either Text Int
encodeOutputIs which wordId caps (Sentence _ _ analogies) = getCode which mAnalogy
    where isWhich (Analogy _ _ _ True) (Analogy _ _ _ v) = v == wordId  -- Is Verb
          isWhich (Analogy _ _ True _) (Analogy _ _ o _) = o == wordId  -- Is Object
          isWhich (Analogy _ True _ _) (Analogy _ s _ _) = s == wordId  -- Is Subject
          isWhich _ _ = False
          mAnalogy = Vector.find (isWhich which) analogies
          
          getCode _ Nothing = Right 0
          getCode (Analogy _ True False True) (Just (Analogy _ s _ _)) = Right (relativeRId caps s wordId) -- Is Verb of Subject s
          getCode (Analogy _ False True True) (Just (Analogy _ _ o _)) = Right (relativeRId caps o wordId) -- Is Verb of Object o
          getCode _ _ = Right 1
                            
--   tensesCap blocks of:
--     9 bit: select AbstractTime start
--     9 bit: select AbstractTime end
encodeTense :: Sentence Int -> Int -> Bool -> Either Text Integer
encodeTense s tenseId isStart = return tense
        where getSentenceTense (Sentence tenses _ _) = tenses Vector.!? tenseId
              propMaybe (Just (t1, t2)) = (Just t1, Just t2)
              propMaybe _ = (Nothing, Nothing)
              (mTenseStart, mTenseEnd) = propMaybe $ getSentenceTense s
              tenseStart = encodeTenseId mTenseStart
              tenseEnd = encodeTenseId mTenseEnd
              tense = if isStart then tenseStart else tenseEnd
    
encodeTenseId :: Maybe AbstractTime -> Integer
encodeTenseId (Just NTMinusInfinity) = 9
encodeTenseId (Just NTMinus) = 8
encodeTenseId (Just NTMinusDelta) = 7
encodeTenseId (Just NTPast) = 6
encodeTenseId (Just NTNow) = 5
encodeTenseId (Just NTFuture) = 4
encodeTenseId (Just NTPlusDelta) = 3
encodeTenseId (Just NTPlus) = 2
encodeTenseId (Just NTPlusInfinity) = 1
encodeTenseId _ = 0
    
ohcTenseWidth :: OneHotCaps -> Int
ohcTenseWidth _ = length (encodeOneHotTense Nothing)

encodeOneHotTense :: Maybe AbstractTime -> [Int]
encodeOneHotTense (Just NTMinusInfinity) = [1,0,0,0,0,0,0,0,0,0]
encodeOneHotTense (Just NTMinus) =         [0,1,0,0,0,0,0,0,0,0]
encodeOneHotTense (Just NTMinusDelta) =    [0,0,1,0,0,0,0,0,0,0]
encodeOneHotTense (Just NTPast) =          [0,0,0,1,0,0,0,0,0,0]
encodeOneHotTense (Just NTNow) =           [0,0,0,0,1,0,0,0,0,0]
encodeOneHotTense (Just NTFuture) =        [0,0,0,0,0,1,0,0,0,0]
encodeOneHotTense (Just NTPlusDelta) =     [0,0,0,0,0,0,1,0,0,0]
encodeOneHotTense (Just NTPlus) =          [0,0,0,0,0,0,0,1,0,0]
encodeOneHotTense (Just NTPlusInfinity) =  [0,0,0,0,0,0,0,0,1,0]
encodeOneHotTense Nothing =                [0,0,0,0,0,0,0,0,0,1]

decodeSentence :: OneHotCaps -> [[Int]] -> [[Int]] -> [[Int]] -> [[Int]] -> Either Text (Sentence Int)
decodeSentence caps ohTenses ohMs ohSoVs ohOoVs = eSentence mTenses mModifiers analogies
    where mTenses = decodeTenses (map Vector.fromList ohTenses)
          mModifiers = decodeModifiers caps ohMs
          mVerbToSubjects = decodeRelationToWords caps ohSoVs
          mVerbToObjects = decodeRelationToWords caps ohOoVs
          verbToSO = combineRelationsToWords mVerbToSubjects mVerbToObjects
          analogies = decodeAnalogies verbToSO
          eSentence Nothing _ _ = Left "Could not decode tenses"
          eSentence (Just _) Nothing _ = Left "Could not decode modifiers"
          eSentence (Just ts) (Just ms) as | null as = Left "Could not decode analogies"
                                           | otherwise = Right (Sentence ts ms as)

decodeTenses :: [Vector Int] -> Maybe Tenses
decodeTenses [t1S, t1E, t2S, t2E] = mTenses mT1S mT1E mT2S mT2E
    where mT1S = decodeOneHotTense t1S
          mT1E = decodeOneHotTense t1E
          mT2S = decodeOneHotTense t2S
          mT2E = decodeOneHotTense t2E
          mTenses (Just s) (Just e) Nothing Nothing = Just $ Vector.singleton (s,e)
          mTenses (Just s1) (Just e1) (Just s2) (Just e2) = Just $ Vector.fromList [(s1,e1),(s2,e2)]
          mTenses _ _ _ _ = Nothing
decodeTenses _ = Nothing

decodeOneHotTense :: Vector Int -> Maybe AbstractTime
decodeOneHotTense bits = decodeTenseId $ Vector.findIndex (== 1) (reverse bits)
decodeTenseId :: Maybe Int -> Maybe AbstractTime
decodeTenseId (Just 9) = Just NTMinusInfinity
decodeTenseId (Just 8) = Just NTMinus
decodeTenseId (Just 7) = Just NTMinusDelta
decodeTenseId (Just 6) = Just NTPast
decodeTenseId (Just 5) = Just NTNow
decodeTenseId (Just 4) = Just NTFuture
decodeTenseId (Just 3) = Just NTPlusDelta
decodeTenseId (Just 2) = Just NTPlus
decodeTenseId (Just 1) = Just NTPlusInfinity
decodeTenseId (Just 0) = Nothing
decodeTenseId _ =        Nothing

decodeModifiers :: OneHotCaps -> [[Int]] -> Maybe (Modifiers Int)
decodeModifiers caps ohMs = mModifiers modifiers
    where modifiers = decodeRelationToWords caps ohMs
          mModifiers Nothing = Nothing
          mModifiers (Just mps) = Just $ Vector.fromList (map (\(m,w) -> Modifier m w) mps)

decodeRelationToWords :: OneHotCaps -> [[Int]] -> Maybe [(Int, Int)]
decodeRelationToWords caps ohWs = Just relations
    where ws = map decodeBitList ohWs
          relations = mapMaybe (decodeRelationToWord (ohcSplit caps)) (zip [0..] ws)

decodeRelationToWord :: Int -> (Int, Int) -> Maybe (Int, Int)
decodeRelationToWord splitCount (wordId, rCode) | rCode == 0 = Nothing
                                                | otherwise = Just $ (wordId, referentId)
    where referentId = rCode + wordId - 1 - splitCount - 1

combineRelationsToWords :: Maybe [(Int,Int)] -> Maybe [(Int,Int)] -> [(Int,Int,Int)]
combineRelationsToWords (Just fs) (Just ss) = mapMaybe (combineRelationToWords fs) ss
combineRelationsToWords _ _ = []

combineRelationToWords :: [(Int, Int)] -> (Int,Int) -> Maybe (Int,Int,Int)
combineRelationToWords wrs (wId, r2) = construct mFound
    where construct Nothing = Nothing
          construct (Just (_, r1)) = Just (wId, r1, r2)
          mFound = find (\(w,_) -> w == wId) wrs

decodeAnalogies :: [(Int,Int,Int)] -> Analogies Int
decodeAnalogies vSOs = Vector.fromList $ map (decodeAnalogy tIdFn) vSOs
    where tIdFn s = fst $ fromMaybe (0,0) (find (\(_,s') -> s == s') subjects)
          subjects = zip [0..] (nub $ map (\(_,s,_) -> s) vSOs)

decodeAnalogy :: (Int -> Int) -> (Int,Int,Int) -> Analogy Int
decodeAnalogy tIdFn (v,s,o) = Analogy (tIdFn s) s o v
          
data WordBitGroup = WordBitGroup Int Int Int Int Int (Vector Int) (Vector Int) deriving (Eq, Show, Read)
data TenseBitGroup = TenseBitGroup (Vector Int) (Vector Int) deriving (Eq, Show, Read)

decodeEncodeOneHotOutput :: OneHotCaps -> Sentence Int -> Int -> Either Text (Sentence Int, Bool)
decodeEncodeOneHotOutput caps s wordCount = do
    encodings <- mapM (\wId -> encodeOutputModifier wId caps s) [0..(wordCount-1)]
    subjectOfVerbs <- mapM (\wId -> encodeOutputIs (Analogy 0 True False True) wId caps s) [0..(wordCount-1)]
    objectOfVerbs <- mapM (\wId -> encodeOutputIs (Analogy 0 False True True) wId caps s) [0..(wordCount-1)]
    tenses <- mapM (\(tId,isStart) -> encodeTense s tId isStart) [(0, True), (0, False), (1, True), (1, False)]
    let ohEncodings = map (padOrdinal (ohcWordWidth caps) . encodeBitList) encodings
        ohSubjectOfVerbs = map (padOrdinal (ohcWordWidth caps) . encodeBitList) subjectOfVerbs
        ohObjectOfVerbs = map (padOrdinal (ohcWordWidth caps) . encodeBitList) objectOfVerbs
        ohTenses = map (padOrdinal (ohcTenseWidth caps) . encodeBitList . ((2::Int) ^)) tenses
    rs <- decodeSentence caps ohTenses ohEncodings ohSubjectOfVerbs ohObjectOfVerbs
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
          modifiers = Vector.fromList $ concatMap newModifiers (subject:analogyObjects)
          analogies = Vector.fromList $ concatMap (newAnalogies (subjectId subject)) analogyObjects
          subjectId (SubjectP (_,_,s,_)) = Just s
          subjectId _ = Nothing
          newModifiers (SubjectP (ms,mees,_,_)) = zipWith Modifier ms mees
          newModifiers (PrimaryAnalogyObjectP t v o) = newModifiers (AnalogyObjectP t v o)
          newModifiers (AnalogyObjectP _ (vms,vmees,_,_) (oms,omees,_,_)) =
              zipWith Modifier (vms++oms) (vmees++omees)
          newModifiers _ = []
          newAnalogies s (PrimaryAnalogyObjectP tId v o) = newAnalogies s (AnalogyObjectP tId v o)
          newAnalogies (Just s) (AnalogyObjectP tId (_,_,v,_) (_,_,o,_)) = [Analogy tId s o v]
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

generateSentence :: Linearizer Text -> WordGenerator Text -> Int -> StdGen -> ((InputSentence, Sentence Int), StdGen)
generateSentence linearizer wg maxModifiers g = evalState (runRandT (generateSentenceM linearizer wg maxModifiers) g) 0
