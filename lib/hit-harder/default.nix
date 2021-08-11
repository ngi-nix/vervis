{ mkDerivation, base, binary, bytestring, containers, cryptonite
, git, hashable, lib, memory, monad-hash, unordered-containers
, zlib
}:
mkDerivation {
  pname = "hit-harder";
  version = "0.1";
  src = ./.;
  libraryHaskellDepends = [
    base binary bytestring containers cryptonite git hashable memory
    monad-hash unordered-containers zlib
  ];
  homepage = "https://dev.angeley.es/s/fr33domlover/p/vervis";
  description = "More Git tools in pure Haskell on top of 'git'";
  license = lib.licenses.publicDomain;
}
