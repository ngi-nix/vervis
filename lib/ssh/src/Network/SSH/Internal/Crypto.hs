module Network.SSH.Internal.Crypto
    ( Cipher (..)
    , CipherType (..)
    , CipherMode (..)
    , HMAC (..)
    , PublicKey (..)
    , KeyPair (..)
    , keyPairFromFile
    , removeKeyPairHeaderFooter
    , addKeyPairHeaderFooter
    , parseKeyPair
    , printKeyPair
    , generator
    , safePrime
    , toBlocks
    , fromBlocks
    , rsaKeyLen
    , blob
    , blobToKey
    , sign
    , actualSignatureLength
    , verify
    )
where

import Control.Monad (replicateM)
import Data.ASN1.BinaryEncoding (BER(..), DER(..))
import Data.ASN1.Encoding (decodeASN1, encodeASN1)
import Data.ASN1.Stream (getConstructedEnd)
import Data.ASN1.Types (ASN1(..), ASN1ConstructionType(..))
import Data.Binary.Get (runGet)
import Data.Binary.Put (runPut)
import Data.Digest.Pure.SHA (bytestringDigest, sha1)
import Data.List (isPrefixOf)
import qualified Codec.Binary.Base64.String as B64
import qualified Codec.Crypto.RSA.Pure as RSA
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBSC (pack, unpack)
import qualified OpenSSL.DSA as DSA

import qualified Crypto.Types.PubKey.RSA as RSAKey

import Network.SSH.Internal.NetGet
import Network.SSH.Internal.NetPut
import Network.SSH.Internal.Util

data Cipher =
    Cipher
        { cType :: CipherType
        , cMode :: CipherMode
        , cBlockSize :: Int
        , cKeySize :: Int
        }

data CipherType = AES
data CipherMode = CBC

data HMAC =
    HMAC
        { hDigestSize :: Int
        , hFunction :: LBS.ByteString -> LBS.ByteString
        }

data PublicKey
    = RSAPublicKey
        { rpubE :: Integer
        , rpubN :: Integer
        }
    | DSAPublicKey
        { dpubP :: Integer
        , dpubQ :: Integer
        , dpubG :: Integer
        , dpubY :: Integer
        }
    deriving (Eq, Show)

data KeyPair
    = RSAKeyPair
        { rprivPub :: PublicKey
        , rprivD :: Integer
        , rprivPrime1 :: Integer
        , rprivPrime2 :: Integer
        , rprivExponent1 :: Integer
        , rprivExponent2 :: Integer
        , rprivCoefficient :: Integer
        }
    | DSAKeyPair
        { dprivPub :: PublicKey
        , dprivX :: Integer
        }
    deriving (Eq, Show)

keyPairFromFile :: FilePath -> IO KeyPair
keyPairFromFile fn = do
    x <- readFile fn
    return $ parseKeyPair x

removeKeyPairHeaderFooter :: [String] -> (String, [String])
removeKeyPairHeaderFooter xs =
   (reverse . drop 17 . reverse . drop 11 . head $ xs, filter (not . ("--" `isPrefixOf`)) xs)

addKeyPairHeaderFooter :: String -> [String] -> [String]
addKeyPairHeaderFooter what xs =
   ["-----BEGIN " ++ what ++ " PRIVATE KEY-----"] ++ xs ++ ["-----END " ++ what ++ " PRIVATE KEY-----"]

-- |Parse an key pair from OpenSSH private key file format.
parseKeyPair :: String -> KeyPair
parseKeyPair x =
    let (what, body) = removeKeyPairHeaderFooter . lines $ x

        asn1 = B64.decode . concat $ body

    in case decodeASN1 BER (LBSC.pack asn1) of
        Right (Start Sequence:ss)
            | all isIntVal (fst $ getConstructedEnd 0 ss) ->
            let (is, _) = getConstructedEnd 0 ss
            in case what of
                 "RSA" ->
                   RSAKeyPair
                     { rprivPub = RSAPublicKey
                        { rpubE = intValAt 2 is
                        , rpubN = intValAt 1 is
                        }
                    , rprivD = intValAt 3 is
                    , rprivPrime1 = intValAt 4 is
                    , rprivPrime2 = intValAt 5 is
                    , rprivExponent1 = intValAt 6 is
                    , rprivExponent2 = intValAt 7 is
                    , rprivCoefficient = intValAt 8 is
                    }
                 "DSA" ->
                   DSAKeyPair
                    { dprivPub = DSAPublicKey
                       { dpubP = intValAt 1 is
                       , dpubQ = intValAt 2 is
                       , dpubG = intValAt 3 is
                       , dpubY = intValAt 4 is
                       }
                    , dprivX = intValAt 5 is
                    }
                 _ -> error ("unknown key type: " ++ what)
        Right u -> error ("unknown ASN1 decoding result: " ++ show u)
        Left e -> error ("ASN1 decoding of private key failed: " ++ show e)
  where
    isIntVal (IntVal _) = True
    isIntVal _ = False

    intValAt i is =
        case is !! i of
            IntVal n -> n
            v -> error ("not an IntVal: " ++ show v)

-- |Turn an key pair into OpenSSH private key file format.
printKeyPair :: KeyPair -> String
printKeyPair keyPair =
  unlines . addKeyPairHeaderFooter what . lines . B64.encode . LBSC.unpack . encodeASN1 DER $ asn1Structure

    where

      (what, asn1Structure) =
        case keyPair of
          (RSAKeyPair { rprivPub = RSAPublicKey { rpubE = e, rpubN = n },
                        rprivD = d, rprivPrime1 = p1, rprivPrime2 = p2,
                        rprivExponent1 = exp1, rprivExponent2 = exp2, rprivCoefficient = c
                      })
             -> ("RSA", [Start Sequence, IntVal 0, IntVal n, IntVal e, IntVal d,
                         IntVal p1, IntVal p2, IntVal exp1, IntVal exp2, IntVal c, End Sequence])

          (DSAKeyPair { dprivPub = DSAPublicKey { dpubP = p, dpubQ = q, dpubG = g, dpubY = y }, dprivX = x })
             -> ("DSA", [Start Sequence, IntVal 0, IntVal p, IntVal q, IntVal g,
                         IntVal y, IntVal x, End Sequence])

          _ -> error "printKeyPair: unsupported key pair"



-- these are the generator and prime for the "Second Oakley Group" described in RFC 2409

generator :: Integer
generator = 2

safePrime :: Integer
safePrime = 179769313486231590770839156793787453197860296048756011706444423684197180216158519368947833795864925541502180565485980503646440548199239100050792877003355816639229553136239076508735759914822574862575007425302077447712589550957937778424442426617334727629299387668709205606050270810842907692932019128194467627007

toBlocks :: (Integral a) => a -> LBS.ByteString -> [LBS.ByteString]
toBlocks _ m | m == LBS.empty = []
toBlocks bs m = b : rest
  where
    b = LBS.take (fromIntegral bs) m
    rest = toBlocks bs (LBS.drop (fromIntegral bs) m)

fromBlocks :: [LBS.ByteString] -> LBS.ByteString
fromBlocks = LBS.concat

rsaKeyLen :: PublicKey -> Int
-- There's no explicit indication of size in the key format so we just
-- have to look at the magnitude of the numbers.
-- This is consistent with what e.g. openssh does.
rsaKeyLen (RSAPublicKey _e n) = (1 + integerLog2 n) `div` 8
rsaKeyLen _ = error "rsaKeyLen: not an RSA public key"

blob :: PublicKey -> LBS.ByteString
blob (RSAPublicKey e n) = runPut $ do
    string "ssh-rsa"
    integer e
    integer n
blob (DSAPublicKey p q g y) = runPut $ do
    string "ssh-dss"
    integer p
    integer q
    integer g
    integer y

blobToKey :: LBS.ByteString -> PublicKey
blobToKey s = flip runGet s $ do
    t <- readString

    case t of
        "ssh-rsa" -> do
            e <- readInteger
            n <- readInteger
            return $ RSAPublicKey e n
        "ssh-dss" -> do
            [p, q, g, y] <- replicateM 4 readInteger
            return $ DSAPublicKey p q g y
        u -> error $ "unknown public key format: " ++ u

sign :: KeyPair -> LBS.ByteString -> IO LBS.ByteString
sign (RSAKeyPair p@(RSAPublicKey e n) d _ _ _ _ _) m = do
  let keyLen = rsaKeyLen p
      sig = RSA.rsassa_pkcs1_v1_5_sign RSA.hashSHA1 (RSAKey.PrivateKey (RSAKey.PublicKey keyLen n e) d 0 0 0 0 0) m
  case sig of
    Right sigBs -> return $ LBS.concat [ netString "ssh-rsa"
                                       , netLBS sigBs
                                       ]
    Left rsaErr -> error $ "Error while performing RSA signature: " ++ show rsaErr
sign (DSAKeyPair (DSAPublicKey p q g y) x) m = do
    (r, s) <- DSA.signDigestedDataWithDSA (DSA.tupleToDSAKeyPair (p, q, g, y, x)) digest
    return $ LBS.concat
        [ netString "ssh-dss"
        , netLBS $ LBS.concat
            [ LBS.pack $ i2osp 20 r
            , LBS.pack $ i2osp 20 s
            ]
        ]
  where
    digest = LBS.toStrict . bytestringDigest . sha1 $ m
sign _ _ = error "sign: invalid key pair"

-- |The length of the actual signature for a given key
-- The actual signature data is always found at the end of a complete signature,
-- so can be extracted by just grabbing this many bytes at the end.
actualSignatureLength :: PublicKey -> Int
actualSignatureLength p@(RSAPublicKey {}) = rsaKeyLen p
actualSignatureLength (DSAPublicKey {}) = 40

verify :: PublicKey -> LBS.ByteString -> LBS.ByteString -> IO Bool
verify p@(RSAPublicKey e n) message signature = do
    let keyLen = rsaKeyLen p
        realSignature = LBS.drop (LBS.length signature - fromIntegral keyLen) signature
        sigRes = RSA.rsassa_pkcs1_v1_5_verify RSA.hashSHA1 (RSAKey.PublicKey keyLen n e) message realSignature
    return $ case sigRes of
               Right r -> r
               Left  _ -> False

verify (DSAPublicKey p q g y) message signature = do
    let realSignature = LBS.drop (LBS.length signature - 40) signature
        r = fromOctets (256 :: Integer) (LBS.unpack (LBS.take 20 realSignature))
        s = fromOctets (256 :: Integer) (LBS.unpack (LBS.take 20 (LBS.drop 20 realSignature)))
    DSA.verifyDigestedDataWithDSA (DSA.tupleToDSAPubKey (p, q, g, y)) digest (r, s)
  where
    digest = LBS.toStrict . bytestringDigest . sha1 $ message
