{
  description = "Keeping track of AOEIV stats";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=24.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, flake-utils, nixpkgs }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        packages.default = pkgs.stdenv.mkDerivation {
          name = "aoeiv-stats";
          nativeBuildInputs = with pkgs.elmPackages; [
            elm
            elm-format
            elm-test
          ];

          src = ./.;

          configurePhase = ''
            export HOME=$PWD/.home
          '';

          buildPhase = ''
            elm make --optimize src/Main.elm
          '';

          checkPhase = ''
            echo -e "\033[1;33mChecking formatting...\033[0m"
            elm-format --validate src
            echo -e "\033[1;32mFormat OK!\033[0m"

            echo -e "Runnings tests..."
            elm-test
          '';
          doCheck = true;

          installPhase = ''
            mkdir -p $out
            cp index.html $out/index.html
          '';
        };
        devShells.default = pkgs.mkShell {
          name = "aoeiv-stats";

          packages = with pkgs.elmPackages; [
            elm
            elm-doc-preview
            elm-format
            elm-optimize-level-2
            elm-test
            elm-language-server
            #pkgs.caddy
            #pkgs.nodejs_20
            #pkgs.nodePackages.terser
            #pkgs.shellcheck
          ];

          # shellHook = ''
          #   export project="$PWD"
          #   export build="$project/.build"
          #   export PATH="$project/bin:$PATH"
          #
          #   npm install --loglevel silent
          # '';
        };
      }
    );
}
