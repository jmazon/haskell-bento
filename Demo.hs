-- Gloss over this for now
{-# LANGUAGE FlexibleInstances #-}
import WaveExport
import Data.Char
import LetItBe

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
-- Important: function definitions
--            recursive (potentially infinite structure)
-- Optional: partial application

-- Demo
-- > dump (sineWave 440)
-- $ aplay -t raw -r 44100 -f FLOAT_LE audiodump

data BPitch = La  | SiB | Si | Do  | DoD | Re
            | MiB | Mi  | Fa | FaD | Sol | LaB | La'
              deriving (Eq,Show,Enum) -- we'll speak about this line later
-- Important: Data Type

-- (initially generated with: take 13 $ iterate (* 2 ** (1/12)) 440)
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
-- Important: pattern matching

-- Demo: play a sine by pitch
-- > dump (sineWave (pitchToFreq Do))

bpScale :: [BPitch]
bpScale = [La,Si,DoD,Re,Mi,FaD,LaB,La']
-- (I *know* about enharmonics, but not now!)

playFreq :: Freq -> Wave
playFreq f = take 22050 (sineWave f)

playBPitch :: BPitch -> Wave
playBPitch p = take 22050 $ sineWave $ pitchToFreq p

playBPList :: [BPitch] -> Wave
playBPList = concatMap playBPitch

-- Demo: play a scale
-- > dump (playBPList bpScale)

-- Important: typeclasses
class Playable p where play :: p -> Wave
instance Playable Freq where play = playFreq
instance Playable BPitch where play = playBPitch

instance Playable p => Playable [p] where play = concatMap play

-- Demo: play pseudo-random frequencies
-- > dump (play [440.0,460..600])

-- Important: Enums (like that list range above)
-- Demo: [La .. La']
pbDemo = [Do, Do, Do, Re, Mi, Mi, Re, Re, Do, Mi, Re, Re, Do, Do, Do, Do]
pbDemo2 = [La', Mi, FaD, DoD, Re, La, Re, Mi]

-- > dump $ play pbDemo
-- > dump $ play $ map succ pbDemo
-- > dump . play . map (pred . pred . pred) $ pbDemo

-- Optional: rewrite pitchToFreq using Enum
-- pitchToFreq' :: BPitch -> Freq
pitchToFreq' p = 440 * 2 ** (n/12)
  where n = fromIntegral $ fromEnum p

-- Handling rests
-- Important: Maybe
instance Playable p => Playable (Maybe p) where
    play (Just p) = play p
    play Nothing = replicate 22050 0

-- Demo: replace some notes in pbDemo with rests
--       then the same with a map call

-- How to transpose a [Maybe BPitch]?
-- Important: Functor

-- Putting it all together
-- Important: record
data Event = Event { eventDuration :: Int
                   , eventNote     :: Maybe Pitch }
             deriving (Show)
type Pitch = (BPitch,Octave)
type Octave = Int

fullPitchToFreq :: Pitch -> Freq
fullPitchToFreq (p,o) = pitchToFreq p * 2 ** (fromIntegral o - 4)
-- Optional: the same using with Enum
noteWave :: Maybe Pitch -> Wave
noteWave n = maybe (repeat 0) sineWave (fmap fullPitchToFreq n)
bpm = 80
instance Playable Event where
    play e = take (round $ rate * 60 / 8 / bpm
                           * (fromIntegral (eventDuration e)))
                  (noteWave (eventNote e))

-- Demo

readBPitch :: Char -> Maybe BPitch
readBPitch p = case p of
                'a' -> Just La
                'A' -> Just SiB
                'b' -> Just Si
                'c' -> Just Do
                'C' -> Just DoD
                'd' -> Just Re
                'D' -> Just MiB
                'e' -> Just Mi
                'f' -> Just Fa
                'F' -> Just FaD
                'g' -> Just Sol
                'G' -> Just LaB
                _   -> Nothing

readDigit :: Char -> Maybe Int
readDigit d | isHexDigit d = Just $ digitToInt d
readDigit _ = Nothing

readEvent :: String -> Maybe Event
readEvent (d:bp:o:[]) = do
  d' <- readDigit d
  case readBPitch bp of
    Just bp' -> do
         o' <- readDigit o
         return $ Event d' (Just (bp', o'))
    _ -> return $ Event d' Nothing
readEvent _ = Nothing

readScore :: String -> Maybe [Event]
readScore = mapM readEvent . words

kanon = "8F4 8e4 8d4 8C4 " ++
        "8b4 8a4 8b4 8C4 " ++
        "8d4 8C4 8b4 8a4 " ++
        "8g3 8F3 8g3 8e3 " ++
        "4d3 4F3 4a4 4g3 4F3 4d3 4F3 4e3 " ++
        "4d3 4b3 4d3 4a4 4g3 4b4 4a4 4g3 " ++
        "4F3 4d3 4e3 4C4 4d4 4F4 4a5 4a4 " ++
        "4b4 4g3 4a4 4F3 4d3 4d4 6d4 2C4 " ++
        "2d4 2C4 2d4 2d3 2C3 2a4 2e3 2F3 2d3 2d4 2C4 2b4 2C4 2F4 2a5 2b5 " ++
        "2g4 2F4 2e4 2g4 2F4 2e4 2d4 2C4 2b4 2a4 2g3 2F3 2e3 2g3 2F3 2e3 " ++
        "2d3 2e3 2F3 2g3 2a4 2e3 2a4 2g3 2F3 2b4 2a4 2g3 2a4 2g3 2F3 2e3 " ++
        "2d3 2b3 2b4 2C4 2d4 2C4 2b4 2a4 2g3 2F3 2e3 2b4 2a4 2b4 2a4 2g3 " ++
        "4F3 4F4 8e4 4__ 4d4 8F4 " ++
        "8b5 8a5 8b5 8C5 " ++
        "4d5 4d4 8C4 4__ 4b4 8d4 " ++
        "Cd4 4d4 4d4 4g4 4e4 4a5 " ++
        "2a5 1F4 1g4 2a5 1F4 1g4 1a5 1a4 1b4 1C4 1d4 1e4 1F4 1g4 2F4 1d4 1e4 2F4 1F3 1g3 1a4 1b4 1a4 1g3 1a4 1F3 1g3 1a4 " ++
        "2g3 1b4 1a4 2g3 1F3 1e3 1F3 1e3 1d3 1e3 1F3 1g3 1a4 1b4 2g3 1b4 1a4 2b4 1C4 1d4 1a4 1b4 1C4 1d4 1e4 1F4 1g4 1a5 " ++
        "2F4 1d4 1e4 2F4 1e4 1d4 1e4 1C4 1d4 1e4 1F4 1e4 1d4 1C4 2d4 1b4 1C4 2d4 1d3 1e3 1F3 1g3 1F3 1e3 1F3 1d4 1C4 1d4 " ++
        "2b4 1d4 1C4 2b4 1a4 1g3 1a4 1g3 1F3 1g3 1a4 1b4 1C4 1d4 2b4 1d4 1C4 2d4 1C4 1b4 1C4 1d4 1e4 1d4 1C4 1d4 1b4 1C4 " ++
        "4d4 4__ 4C4 4__ 4b4 4__ 4d4 4__ " ++
        "4d3 4__ 4d3 4__ 4d3 4__ 4e3 4__ " ++
        "4__ 4a4 4__ 4a4 4__ 4F3 4__ 4a4 " ++
        "4__ 4g3 4__ 4F3 4__ 4g3 4__ 4e4" :: String

main = undefined

-- ugly support code (would be less ugly if we could remove La' from
-- BPitch, but that would mess up with too many of the early examples)
instance Enum Pitch where
    succ (La',o) = (SiB,o+1) -- special case for pbDemo
    succ (LaB,o) = (La,o+1)
    succ (bp,o)  = (succ bp,o)
    pred (La,o)  = (LaB,o-1)
    pred (bp,o)  = (pred bp,o)
    fromEnum (bp,o) = 12*o + fromEnum bp
    toEnum n = (toEnum bp,o) where (o,bp) = n `divMod` 12

-- Demo: polyphony
eDemo2 = map (\bp -> Event 16 (Just (bp,3))) pbDemo2
eDemo2' = map (\(Event d n) -> Event d (fmap (succ . succ . succ) n)) eDemo2
-- > let Just lib = fmap play (readScore letItBe)
-- > let m = map (/2) $ zipWith (+) lib (play (Event 4 Nothing : eDemo2'))
-- > dump m

-- Demo: kanon
eDemo2'' = map (\bp -> Event 8 (fmap (succ . succ . succ . succ . succ) (Just (bp,3)))) pbDemo2
ovb (pb,o) = (pb,o-1)
-- > let m = map (/2) $ zipWith (+) (play $ cycle eDemo2'') (play (readScore kanon))
-- > dump m
