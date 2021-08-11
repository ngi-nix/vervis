{ mkDerivation, base, binary, bytestring, containers, git
, hit-graph, hit-harder, lib, transformers
}:
mkDerivation {
  pname = "hit-network";
  version = "0.1";
  src = ./.;
  libraryHaskellDepends = [
    base binary bytestring containers git hit-graph hit-harder
    transformers
  ];
  homepage = "https://dev.angeley.es/s/fr33domlover/p/vervis";
  description = "Git protocol implementation in pure Haskell";
  license = lib.licenses.publicDomain;
}
