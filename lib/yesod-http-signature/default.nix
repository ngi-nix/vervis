{ mkDerivation, base, bytestring, case-insensitive, cryptonite
, http-client, http-client-signature, http-signature, http-types
, lib, memory, monad-logger, network-uri, text, time, transformers
, wai, yesod-core
}:
mkDerivation {
  pname = "yesod-http-signature";
  version = "0.1";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring case-insensitive http-signature http-types
    monad-logger text time wai yesod-core
  ];
  testHaskellDepends = [
    base cryptonite http-client http-client-signature http-signature
    http-types memory network-uri text time transformers yesod-core
  ];
  homepage = "https://dev.angeley.es/s/fr33domlover/p/vervis";
  description = "Cryptographic request verification for Yesod web apps";
  license = lib.licenses.publicDomain;
}
