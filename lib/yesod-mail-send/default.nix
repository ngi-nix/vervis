{ mkDerivation, aeson, base, email-validate, lib, mime-mail
, monad-logger, network, shakespeare, smtp-mail, text, yesod-core
}:
mkDerivation {
  pname = "yesod-mail-send";
  version = "0.1";
  src = ./.;
  libraryHaskellDepends = [
    aeson base email-validate mime-mail monad-logger network
    shakespeare smtp-mail text yesod-core
  ];
  homepage = "https://dev.angeley.es/s/fr33domlover/r/yesod-mail-send";
  description = "Send email from your Yesod web app";
  license = lib.licenses.publicDomain;
}
