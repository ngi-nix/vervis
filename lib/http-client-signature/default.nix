{ mkDerivation, base, bytestring, case-insensitive, cryptonite
, http-client, http-date, http-signature, http-types, lib, memory
, network-uri, time, transformers, wai, warp
}:
mkDerivation {
  pname = "http-client-signature";
  version = "0.1";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring case-insensitive http-client http-date
    http-signature http-types time
  ];
  testHaskellDepends = [
    base bytestring case-insensitive cryptonite http-client
    http-signature http-types memory network-uri time transformers wai
    warp
  ];
  homepage = "https://dev.angeley.es/s/fr33domlover/p/vervis";
  description = "Cryptographic signing of HTTP requests";
  license = lib.licenses.publicDomain;
}
