{ inputs, ... }:
{
  perSystem =
    { pkgs
    , inputs'
    , self'
    , system
    , ...
    }:
    {
      # _module.args.pkgs = import inputs.nixpkgs {
      #   inherit system;
      #   overlays = [
      #     inputs.ctl.overlays.runtime
      #   ];
      # };

      devShells.default = pkgs.mkShell {
        buildInputs = [
          # self'.packages.cardano-node
          pkgs.nixpkgs-fmt
          # inputs'.aiken.packages.aiken
          # pkgs.nixos-rebuild
          # pkgs.nix-diff
          # pkgs.dia
          # pkgs.kazam
          # pkgs.audacity
          # pkgs.shotcut
          pkgs.nixd
          # pkgs.rust-analyzer
          # pkgs.cargo
          # pkgs.fileshare
          # self'.packages.arion-with-prebuilt
          # pkgs.ogmios
          # pkgs.cardano-node
          # pkgs.cardano-cli
          # pkgs.spago
          # pkgs.purescript
          # pkgs.easy-ps
        ] 
        ++
        self'.devShells.ctl.buildInputs
        ;
        # (with pkgs.python310Packages; [ jupyterlab scikit-learn pandas psycopg2 matplotlib tabulate ]) ++
        # shellHook = ''
        #   export SSHOPTS="-p 2222"
        # ''
        
      };

      devShells.purs =
        let
          easy-ps = inputs.easy-purescript-nix.packages.${system};
        in
        pkgs.mkShell {
          name = "purescript-custom-shell";
          buildInputs = [
            easy-ps.purs-0_14_5
            easy-ps.spago
            easy-ps.purescript-language-server
            easy-ps.purs-tidy
            pkgs.nodejs-18_x
            pkgs.esbuild
          ];
          shellHook = ''
            source <(spago --bash-completion-script `which spago`)
            source <(node --completion-bash)
          '';
        };
    };
}
