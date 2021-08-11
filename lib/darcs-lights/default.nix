{ mkDerivation, attoparsec, base, base16-bytestring, bytestring
, bytestring-lexing, cryptonite, filepath, lib, memory, text, time
, zlib
}:
mkDerivation {
  pname = "darcs-lights";
  version = "0.1";
  src = ./.;
  libraryHaskellDepends = [
    attoparsec base base16-bytestring bytestring bytestring-lexing
    cryptonite filepath memory text time zlib
  ];
  homepage = "https://dev.angeley.es/s/fr33domlover/r/darcs-lights";
  description = "Some tools for working with Darcs repos";
  license = lib.licenses.publicDomain;
}
