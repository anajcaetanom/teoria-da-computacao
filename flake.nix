{
  description = "Ambiente Haskell para trabalho de automatos";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
    in
    {
      devShells.${system}.default = pkgs.mkShell {
        buildInputs = [
          pkgs.ghc
          pkgs.cabal-install
          pkgs.haskellPackages.yaml
          pkgs.haskellPackages.aeson
          pkgs.haskellPackages.text
          pkgs.haskellPackages.containers
          pkgs.haskellPackages.vector
          pkgs.haskellPackages.scientific
        ];

        shellHook = ''
          echo "Ambiente Haskell carregado."
          echo "Use: cabal run"
        '';
      };
    };
}