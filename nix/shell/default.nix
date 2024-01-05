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
          self'.packages.cardano-node
          pkgs.nixpkgs-fmt
          inputs'.aiken.packages.aiken
          pkgs.nixos-rebuild
        ] ++
        (with pkgs.python310Packages; [ jupyterlab scikit-learn pandas psycopg2 matplotlib tabulate ]) ++
        self'.devShells.ctl.buildInputs;
      };
    };
}
