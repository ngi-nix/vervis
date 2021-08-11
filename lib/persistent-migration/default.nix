{ mkDerivation, base, bytestring, conduit, containers, lib
, megaparsec, parser-combinators, persistent, persistent-template
, template-haskell, text, th-lift-instances, transformers
}:
mkDerivation {
  pname = "persistent-migration";
  version = "0.1";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring conduit containers megaparsec parser-combinators
    persistent persistent-template template-haskell text
    th-lift-instances transformers
  ];
  homepage = "https://dev.angeley.es/s/fr33domlover/p/vervis";
  description = "Specify DB migrations in terms of your persistent model";
  license = lib.licenses.publicDomain;
}
