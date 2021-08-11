{
  description = "vervis";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        haskellPackages = pkgs.haskellPackages;

        jailbreakUnbreak = pkg:
          pkgs.haskell.lib.doJailbreak (pkg.overrideAttrs (_: { meta = { }; }));
          
        packageName = "vervis";
      in {
        packages.${packageName} =
          haskellPackages.callCabal2nix packageName self rec {
            # Dependency overrides go here
            darcs-lights = pkgs.callPackage ./lib/darcs-lights {};
            darcs-rev = pkgs.callPackage ./lib/darcs-rev {};
            dvara = pkgs.callPackage ./lib/dvara {};
            hit-network = pkgs.callPackage ./lib/hit-network {};
            hit-harder = pkgs.callPackage ./lib/hit-harder {};
            http-client-signature = pkgs.callPackage ./lib/http-client-signature {};
            http-signature = pkgs.callPackage ./lib/http-signature {};
            persistent-email-address = pkgs.callPackage ./lib/persistent-email-address {};
            persistent-graph = pkgs.callPackage ./lib/persistent-graph {};
            time-interval-aeson = pkgs.callPackage ./lib/time-interval-aeson {};
            yesod-http-signature = pkgs.callPackage ./lib/yesod-http-signature {};
            yesod-mail-send = pkgs.callPackage ./lib/yesod-mail-send {};
          };

        defaultPackage = self.packages.${system}.${packageName};

        devShell = pkgs.mkShell {
          buildInputs = with haskellPackages; [
            haskell-language-server
            ghcid
            cabal-install

          ];
          inputsFrom = builtins.attrValues self.packages.${system};
        };
      });
}
