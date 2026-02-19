{
  description = "LaTeX development environment";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      nixpkgs,
      flake-utils,
      ...
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            opam
          ];

          shellHook = ''
            if [ ! -d "~/.opam" ]; then
              opam init --bare -y --disable-sandboxing --quiet
            fi

            if ! opam switch show >/dev/null 2>&1; then
              echo "Creating opam switch (this may take a few minutes)..."
              opam switch create . 5.4.0 -y --quiet
            fi

            echo "Syncing tools: ocaml-lsp-server, dune, utop..."
            opam install dune utop ocaml-lsp-server ocamlformat -y

            eval $(opam env)
          '';
        };
      }
    );
}
