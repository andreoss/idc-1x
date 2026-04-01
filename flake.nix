{
  description = "idc-catalog REST services";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

  outputs = { self, nixpkgs }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" ];
      forAll = f: nixpkgs.lib.genAttrs systems (s: f s);
      hpFor = pkgs: pkgs.haskell.packages.ghc96.override {
        overrides = self: super: {
          mkDerivation = args:
            super.mkDerivation (args // { doHaddock = false; doCheck = false; });
        };
      };
    in
    {
      packages = forAll (sys:
        let pkgs = nixpkgs.legacyPackages.${sys};
        in {
          default = (hpFor pkgs).callCabal2nix "idc-catalog" ./. { };
        });

      apps = forAll (sys:
        let pkgs = nixpkgs.legacyPackages.${sys};
        in {
          default = {
            type = "app";
            program = "${self.packages.${sys}.default}/bin/idc-catalog";
          };
        });

      devShells = forAll (sys:
        let pkgs = nixpkgs.legacyPackages.${sys};
            hpDev = pkgs.haskell.packages.ghc96;
        in {
          default = pkgs.mkShell {
            buildInputs = [
              hpDev.ghc
              hpDev.cabal-install
              hpDev.hlint
              hpDev.weeder
              hpDev.haskell-language-server
              pkgs.postgresql_16
              pkgs.redis
              pkgs.curl
              pkgs.pkg-config
              pkgs.zlib
            ];
          };
        });
    };
}
