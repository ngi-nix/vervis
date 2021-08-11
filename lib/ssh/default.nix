{ mkDerivation, asn1-encoding, asn1-types, base, base64-string
, binary, bytestring, cereal, containers, crypto-api
, crypto-pubkey-types, cryptohash-cryptoapi, directory, DRBG
, filepath, HsOpenSSL, HUnit, integer-gmp, lib, libssh2
, monad-control, monadcryptorandom, network, process, pseudomacros
, QuickCheck, random, RSA, SHA, SimpleAES, split, tasty
, tasty-hunit, tasty-quickcheck, template-haskell
, th-lift-instances, transformers, transformers-base, unliftio-core
}:
mkDerivation {
  pname = "ssh";
  version = "0.3.2";
  src = ./.;
  libraryHaskellDepends = [
    asn1-encoding asn1-types base base64-string binary bytestring
    cereal containers crypto-api crypto-pubkey-types
    cryptohash-cryptoapi DRBG HsOpenSSL integer-gmp monad-control
    monadcryptorandom network process random RSA SHA SimpleAES split
    transformers transformers-base unliftio-core
  ];
  testHaskellDepends = [
    base bytestring containers directory filepath HUnit libssh2
    pseudomacros QuickCheck tasty tasty-hunit tasty-quickcheck
    template-haskell th-lift-instances
  ];
  doCheck = false;
  homepage = "http://hub.darcs.net/ganesh/ssh";
  description = "A pure-Haskell SSH server library";
  license = lib.licenses.bsd3;
}
