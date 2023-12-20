{
  perSystem =
    { pkgs
    , inputs'
    , self'
    , ...
    }:
    {
      devShells.default = pkgs.mkShell {
        buildInputs = [
          inputs'.cardano-node.legacyPackages.cardano-cli
          inputs'.cardano-node.legacyPackages.cardano-node
          pkgs.nixpkgs-fmt
        ] ++
        (with pkgs.python310Packages; [ jupyterlab pandas psycopg2 matplotlib tabulate ]) ++
        self'.devShells.ctl.buildInputs;
      };
    };
}
