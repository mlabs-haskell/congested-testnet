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

      devShells.default = pkgs.mkShell {
        buildInputs = with pkgs; [
          nixpkgs-fmt
          # inputs'.aiken.packages.aiken
          nixos-rebuild
          # pkgs.nix-diff
          # pkgs.dia
          # pkgs.kazam
          # pkgs.audacity
          # pkgs.shotcut
          nixd
          # pkgs.rust-analyzer
          # pkgs.cargo
          # pkgs.fileshare
          # self'.packages.arion-with-prebuilt
          arion-with-prebuilt
          cardano-node
          # pkgs.cardano-cli
          # pkgs.spago
          # pkgs.purescript
          # pkgs.nodejs_18
          # pkgs.compiled
          # cardano-cli
          # haskellPackages.cabal-install
          pyright
          nodePackages.svelte-language-server
          nodePackages.typescript-language-server
          nodejs
          ogmios
          kupo
        ] 
        ++
        self'.devShells.ctl.buildInputs
        # ++
        # (with pkgs.python310Packages; [ jupyterlab scikit-learn pandas psycopg2 matplotlib tabulate ])
        ;
        shellHook = ''
          export SSHOPTS="-p 2222"
          export walletPath="/tmp/wallet/wallet.skey";
          export kupoUrl="0.0.0.0"
          export ogmiosUrl="0.0.0.0"
        '';
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
