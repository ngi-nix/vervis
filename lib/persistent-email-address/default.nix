{ mkDerivation, base, bytestring, email-validate, lib, persistent
, text
}:
mkDerivation {
  pname = "persistent-email-address";
  version = "0.1";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring email-validate persistent text
  ];
  homepage = "https://dev.angeley.es/s/fr33domlover/r/persistent-email-address";
  description = "Use EmailAddress from email-validate in your DB";
  license = lib.licenses.publicDomain;
}
