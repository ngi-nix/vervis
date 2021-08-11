{ mkDerivation, base, bytestring, darcs-lights, lib
, template-haskell, text, time
}:
mkDerivation {
  pname = "darcs-rev";
  version = "0.1";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring darcs-lights template-haskell text time
  ];
  homepage = "https://dev.angeley.es/s/fr33domlover/r/darcs-rev";
  description = "Compile Darcs revision info into your project";
  license = lib.licenses.publicDomain;
}
