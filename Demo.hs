-- Gloss over this for now
{-# LANGUAGE TypeSynonymInstances #-}
import WaveExport

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

-- Play a sine by pitch
-- > dump (sineWave (pitchToFreq Do))

-- Play a scale
bpScale :: [BPitch]
bpScale = [La,Si,DoD,Re,Mi,FaD,LaB,La']
-- (I *know* about enharmonics, but not now!)

playFreq :: Freq -> Wave
playFreq f = take 22050 (sineWave f)

playBPitch :: BPitch -> Wave
playBPitch p = take 22050 $ sineWave $ pitchToFreq p

playBPList :: [BPitch] -> Wave
playBPList = concatMap playBPitch

-- Important: typeclasses
class Playable p where play :: p -> Wave
instance Playable Freq where play = playFreq
instance Playable BPitch where play = playBPitch

instance Playable p => Playable [p] where play = concatMap play


-- pitchToFreq' :: BPitch -> Freq
pitchToFreq' p = 440 * 2 ** (n/12)
  where n = fromIntegral $ fromEnum p

freqToNote :: Freq -> Wave
freqToNote f = take 22050 (sineWave f)
sSongToWave = concatMap freqToNote

ssSample = [Do, Do, Do, Re, Mi, Mi, Re, Re, Do, Mi, Re, Re, Do, Do, Do, Do]
-- dump (sSongToWave (pitchesToSSong ssSample))
pbSample = [La', Mi, FaD, DoD, Re, La, Re, Mi]


-- TBD: find something on abcnotation.com that would mingle with pbSSong
--      implement rests
--      implement durations
