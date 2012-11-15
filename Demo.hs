import Data.Binary.Put
import Data.Binary.IEEE754
import qualified Data.ByteString.Lazy as B
import Data.Char

type Freq = Float
type Sample = Float
type Wave = [Sample]

rate = 44100

indexToPhase n = 2*pi * n / rate
makeSample f n = sin(f * indexToPhase(n)) -- expressions can look like C

sineWave :: Freq -> Wave
sineWave f = map (makeSample f) [0..]     -- partial application

-- skip on this or move to other file
serialize w = B.writeFile "audiodump" (runPut (mapM_ putFloat32le w))

-- $ aplay -t raw -r 44100 -f FLOAT_LE audiodump

data BPitch = La  | SiB | Si | Do  | DoD | Re
            | MiB | Mi  | Fa | FaD | Sol | LaB | La'
              deriving (Show, Enum)

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

-- pitchToFreq' :: BPitch -> Freq
pitchToFreq' p = 440 * 2 ** (n/12)
  where n = fromIntegral $ fromEnum p

freqToNote :: Freq -> Wave
freqToNote f = take 22050 (sineWave f)
sSongToWave = concatMap freqToNote

ssSample = [Do, Do, Do, Re, Mi, Mi, Re, Re, Do, Mi, Re, Re, Do, Do, Do, Do]
-- serialize (sSongToWave (pitchesToSSong ssSample))
pbSample = [La', Mi, FaD, DoD, Re, La, Re, Mi]


-- TBD: find something on abcnotation.com that would mingle with pbSSong
--      implement rests
--      implement durations

data Event = Event Int (Maybe Pitch)
             deriving (Show)

readPitch :: Char -> Maybe Pitch
readPitch p = case p of
                'A' -> Just La
                _   -> Nothing

readDigit :: Char -> Maybe Int
readDigit d | isDigit d = Just $ (ord d) - (ord '0')
readDigit _ = Nothing

readEvent :: String -> Maybe Event
readEvent (d:p:[]) = case readDigit d of
                       Just d' -> Just $ Event d' (readPitch p)
                       _       -> Nothing
readEvent _ = Nothing

readScore :: String -> Maybe [Event]
readScore = mapM readEvent . words
