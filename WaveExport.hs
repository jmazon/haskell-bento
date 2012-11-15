module WaveExport (dump,dumpToFile) where

import Data.Binary.Put
import Data.Binary.IEEE754
import qualified Data.ByteString.Lazy as B

dump = dumpToFile "audiodump"
dumpToFile f w = B.writeFile f (runPut (mapM_ putFloat32le w))
