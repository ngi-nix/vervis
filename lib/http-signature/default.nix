{ mkDerivation, attoparsec, base, base64-bytestring, bytestring
, case-insensitive, cryptonite, http-date, http-types, lib, memory
, time
}:
mkDerivation {
  pname = "http-signature";
  version = "0.1";
  src = ./.;
  libraryHaskellDepends = [
    attoparsec base base64-bytestring bytestring case-insensitive
    http-date http-types time
  ];
  testHaskellDepends = [
    base cryptonite http-date http-types memory time
  ];
  homepage = "https://dev.angeley.es/s/fr33domlover/p/vervis";
  description = "Cryptographic signing and verification for HTTP requests";
  license = lib.licenses.publicDomain;
}
