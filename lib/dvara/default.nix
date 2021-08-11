{ mkDerivation, aeson, base, base64-bytestring, cryptonite
, http-types, lib, network-uri, persistent, persistent-migration
, persistent-template, shakespeare, template-haskell, text, time
, transformers, yesod-auth, yesod-core, yesod-form
, yesod-persistent
}:
mkDerivation {
  pname = "dvara";
  version = "0.1";
  src = ./.;
  libraryHaskellDepends = [
    aeson base base64-bytestring cryptonite http-types network-uri
    persistent persistent-migration persistent-template shakespeare
    template-haskell text time transformers yesod-auth yesod-core
    yesod-form yesod-persistent
  ];
  homepage = "https://dev.angeley.es/s/fr33domlover/p/vervis";
  description = "OAuth2 server subsite for Yesod web apps";
  license = lib.licenses.publicDomain;
}
