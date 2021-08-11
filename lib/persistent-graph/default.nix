{ mkDerivation, base, bytestring, conduit, lib, mtl, persistent
, resourcet, text, transformers
}:
mkDerivation {
  pname = "persistent-graph";
  version = "0.1";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring conduit mtl persistent resourcet text transformers
  ];
  homepage = "https://dev.angeley.es/s/fr33domlover/p/vervis";
  description = "Representing, querying and editing graphs with Persistent";
  license = lib.licenses.publicDomain;
}
