module Network.SSH.Internal.Random
    ( RNG ()
    , newRNG
    , gen
    , genBS
    , genInRange
    )
where

import Control.Arrow (second)
import Control.Monad.CryptoRandom (CRandom (..), CRandomR (..))
import Crypto.Random (genBytes, newGenIO, GenError)
import Crypto.Random.DRBG (CtrDRBG)
import Crypto.Util (throwLeft)
import Data.ByteString (ByteString)

type Impl = CtrDRBG

newtype RNG = RNG { unRNG :: Impl }

newRNG :: IO RNG
newRNG = RNG <$> newGenIO

wrap :: Either GenError (a, Impl) -> (a, RNG)
wrap = second RNG . throwLeft

gen :: CRandom a => RNG -> (a, RNG)
gen (RNG rng) = wrap $ crandom rng

genBS :: Int -> RNG -> (ByteString, RNG)
genBS len (RNG rng) = wrap $ genBytes len rng

genInRange :: CRandomR a => (a, a) -> RNG -> (a, RNG)
genInRange range (RNG rng) = wrap $ crandomR range rng
