{
  description = "3110";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-23.05";
  };

  outputs = { nixpkgs, ... }:
  let
    system = "x86_64-linux";
    pkgs = nixpkgs.legacyPackages.${system};
  in {
    devShells.${system}.default = (pkgs.mkShell {
        nativeBuildInputs = with pkgs; [
          ocaml
          dune_3
          exercism
        ] ++ ( with ocamlPackages; [
          utop
          odoc
          ounit2
          qcheck
          menhir
          ocamlformat
          ppxlib
          base
          core_kernel
        ]);
      }
    );
  };
}
