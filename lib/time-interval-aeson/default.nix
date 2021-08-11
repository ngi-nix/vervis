{ mkDerivation, aeson, base, lib, text, time-interval, time-units
}:
mkDerivation {
  pname = "time-interval-aeson";
  version = "0.1";
  src = ./.;
  libraryHaskellDepends = [
    aeson base text time-interval time-units
  ];
  homepage = "https://dev.angeley.es/s/fr33domlover/r/time-interval-aeson";
  description = "Specify time intervals in JSON and YAML";
  license = lib.licenses.publicDomain;
}
