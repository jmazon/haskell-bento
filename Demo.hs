-- Gloss over this for now

{-# LANGUAGE FlexibleInstances #-}
import WaveExport
import Data.Char
import Samples

-- Accoustics basics
type Sample = Float
type Wave = [Sample]
rate = 44100

-- Sine wave generation
type Freq = Float
indexToPhase n = 2*pi * n / rate          -- voluntarily no signature
makeSample f n = sin(f * indexToPhase(n)) -- expressions can look like C

sineWave :: Freq -> Wave
sineWave f = map (makeSample f) [0..]

-- Discussion: function definitions; potentially infinite structure
-- In this example, function makeSample went through partial application.

-- Demo
-- > dump (sineWave 440)
-- $ aplay -t raw -r 44100 -f FLOAT_LE audiodump

data BPitch = La  | SiB | Si | Do  | DoD | Re
            | MiB | Mi  | Fa | FaD | Sol | LaB | La'
              deriving (Eq,Show,Enum) -- we'll speak about this line later

-- Discussion: Data Type definition, with no-argument constructors

pitchToFreq :: BPitch -> Freq
pitchToFreq La  = 440.0
pitchToFreq SiB = 466.1637615180899
pitchToFreq Si  = 493.8833012561241
pitchToFreq Do  = 523.2511306011974
pitchToFreq DoD = 554.3652619537443
pitchToFreq Re  = 587.3295358348153
pitchToFreq MiB = 622.253967444162
pitchToFreq Mi  = 659.2551138257401
pitchToFreq Fa  = 698.456462866008
pitchToFreq FaD = 739.988845423269
pitchToFreq Sol = 783.9908719634989
pitchToFreq LaB = 830.6093951598906
pitchToFreq La' = 880.0000000000003
-- (initially generated with: take 13 $ iterate (* 2 ** (1/12)) 440)

-- Discussion: pattern matching

-- Demo: play a sine by pitch
-- > dump (sineWave (pitchToFreq Do))

bpScale :: [BPitch]
bpScale = [La,Si,DoD,Re,Mi,FaD,LaB,La']

playFreq :: Freq -> Wave
playFreq f = take 22050 (sineWave f)

playBPitch :: BPitch -> Wave
playBPitch p = take 22050 $ sineWave $ pitchToFreq p

playBPList :: [BPitch] -> Wave
playBPList = concatMap playBPitch

-- Demo: play a scale
-- > dump (playBPList bpScale)

-- Discussion: Typeclasses
class Playable p where play :: p -> Wave
instance Playable Freq where play = playFreq
instance Playable BPitch where play = playBPitch

instance Playable p => Playable [p] where play = concatMap play

-- Demo: play pseudo-random frequencies
-- > dump (play [440.0,460..600])

-- Discussion: Enums (like that list range above, if not seen before yet)
-- Demo: chromatic scale [La .. La']
pbDemo = [Do, Do, Do, Re, Mi, Mi, Re, Re, Do, Mi, Re, Re, Do, Do, Do, Do]
pbDemo2 = [La', Mi, FaD, DoD, Re, La, Re, Mi]

-- > dump $ play pbDemo
-- > dump $ play $ map succ pbDemo
-- > dump . play . map (pred . pred . pred) $ pbDemo

-- Exercice and solution: rewrite pitchToFreq using Enum
pitchToFreq' :: BPitch -> Freq
pitchToFreq' p = let n = fromIntegral (fromEnum p)
                 in 440 * 2 ** (n/12)

-- Discussion: Maybe (SIGSEGV, NPE)
instance Playable p => Playable (Maybe p) where
    play (Just p) = play p
    play Nothing = replicate 22050 0
-- This can be used to integrate rests in our musical framework!

-- Demo: replace some notes in pbDemo with rests
--       then the same with a map call

-- Exercice: “edit” one of the example tunes using list functions map
-- and filter:
--   * delete all instance of the note Mi
--   * replace them by rests of the same duration

-- Discussion: Functor
-- How would one transpose a [Maybe BPitch]?

-- Discussion: record types
-- Putting it all together
data Event = Event { eventDuration :: Int
                   , eventNote     :: Maybe Pitch }
             deriving Show
type Pitch = (BPitch,Octave)
type Octave = Int
-- Note that calling instances of type Maybe Pitch notes is a stretch.
-- But AFAIK the classic musical ontology doesn't have a dedicated
-- word for “note or rest”.

fullPitchToFreq :: Pitch -> Freq
fullPitchToFreq (p,o) = pitchToFreq p * 2 ** (fromIntegral o - 4)
-- Exercice: the same using with Enum.
-- Exercice: it can be made much more generic (Bounded b instead of
-- BPitch) if the last note is dropped from the datatype.

noteWave :: Maybe Pitch -> Wave
noteWave n = maybe (repeat 0) sineWave (fmap fullPitchToFreq n)

bpm = 60 -- beats per minute

instance Playable Event where
    play e = take nbSamples freq
        where nbSamples = round (rate * 60 / 8 / bpm
                                 * (fromIntegral (eventDuration e)))
              freq = noteWave (eventNote e)
-- Note how the record syntax provided us with a free getter: eventDuration

-- The rest of the code here implements a score parser, to ease longer
-- music pieces entry.  Read the code or see the examples for the
-- precice syntax.

-- Demonstrate a pattern guard
readBPitch :: Char -> Maybe BPitch
readBPitch p | isUpper p = fmap succ (readBPitch (toLower p))
readBPitch 'a' = Just La
readBPitch 'b' = Just Si
readBPitch 'c' = Just Do
readBPitch 'd' = Just Re
readBPitch 'e' = Just Mi
readBPitch 'f' = Just Fa
readBPitch 'g' = Just Sol
readBPitch  _  = Nothing

-- Demonstrate an if statement
readDigit :: Char -> Maybe Int
readDigit d = if isHexDigit d then Just $ digitToInt d else Nothing

-- Demonstrate
--   * more elaborate pattern matching
--   * do-notation to propagate Maybe state
--   * a case statement
readEvent :: String -> Maybe Event
readEvent (d:bp:o:[]) = do
  d' <- readDigit d
  case readBPitch bp of
    Just bp' -> do
      o' <- readDigit o
      Just $ Event d' $ Just (bp', o')
    Nothing -> Just (Event d' Nothing)
readEvent _ = Nothing

-- Ewww… hard to tell what this one demonstrates without uttering the M-word ;-)
readScore :: String -> Maybe [Event]
readScore = mapM readEvent . words

-- ugly support code (would be less ugly if we could remove La' from
-- BPitch, but that would mess up with too many of the early examples)
instance Enum Pitch where
    succ (La',o) = (SiB,o+1) -- needed specifically for pbDemo
    succ (LaB,o) = (La,o+1)
    succ (bp,o)  = (succ bp,o)
    pred (La,o)  = (LaB,o-1)
    pred (bp,o)  = (pred bp,o)
    fromEnum (bp,o) = 12*o + fromEnum bp
    toEnum n = (toEnum bp,o) where (o,bp) = n `divMod` 12

-- Demo: score reader
-- > letItBe
-- > readScore letItBe
-- > dump (readScore letItBe)

-- Demo: polyphony.  Mingle a delayed transposed (lifted to Event)
-- version of pbDemo2 with the previous example.
-- This demonstrates:
--   * the cons (:) operator
--   * the local function idiom
letItBeBase = Event 4 Nothing : map f pbDemo2
    where f bp = Event 16 (transpose $ bPitchToPitch bp)
          bPitchToPitch bp = Just (bp,3)
          transpose = fmap (succ . succ . succ) -- la -> do

mix :: Wave -> Wave -> Wave
mix w1 w2 = map (/2) $ zipWith (+) w1 w2

polyPhonyDemo = mix (play $ readScore letItBe) (play letItBeBase)
-- > dump polyPhonyDemo

-- Demo: some well-known canon tune
Just canonVoice = readScore kanon
canonBase = map f  pbDemo2
    where f bp = Event 8 (transpose $ bPitchToPitch bp)
          bPitchToPitch bp = Just (bp,3)
          transpose = fmap (succ . succ . succ . succ . succ) -- la -> re

dumpCanonWave = dump (mix i1 i2) where
    -- creating long lists of Floats in GHCi could prove deadly
    vb, v1, v2, v3 :: [Event]
    vb = cycle canonBase
    v1 = Event 64 Nothing : canonVoice
    v2 = Event 64 Nothing : v1
    v3 = Event 64 Nothing : v2
    i1 = mix (play vb) (play v1)
    i2 = mix (play v2) (play v3)
-- > dumpCanonWave

-- Exercices while you have a sound generation framework at hand.
--   * The pitch-to-frequency mappings are currently defined in the
--     equal temperament.  The canon example in the end would
--     definitely sound better in D-centered just intonation.  Modify
--     the fullPitchToFreq and/or the pitchToFreq function to
--     implement
--     https://en.wikipedia.org/wiki/Just_intonation#Diatonic_scale
--   * Implement basic instruments by adding harmonic frequencies to
--     the base, and a sound envelope generator.
--   * Implement drums.  A way to go would be implementing a
--     pseudo-random number generator to simulate white noise.
--     (Actual random numbers are not for beginners in a pure
--     language.)
