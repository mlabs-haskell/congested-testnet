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
          nixos-rebuild
          pkgs.rust-analyzer
          cardano-node
          pyright
          nodePackages.svelte-language-server
          nodePackages.typescript-language-server
          nodePackages.prettier
          nodejs
          ogmios
          kupo
          spammer
          prometheus
        ]
        ++
        self'.devShells.ctl.buildInputs
          # ++
          # (with pkgs.python310Packages; [ jupyterlab scikit-learn pandas psycopg2 matplotlib tabulate ])
        ;
        shellHook = ''
          export SSHOPTS="-p 2222"
          export WALLET_SKEY_PATH="/home/maxim/work/projects/congested-testnet/testnet_data/wallet.skey"
          export CARDANO_NODE_METRICS_URL="0.0.0.0:12789"
          export SPAMMER_METRIC_PORT=8001
          export FAUCET_PORT=8000
          export N_WORKERS=2
          export OGMIOS_URL=0.0.0.0
          export KUPO_URL=0.0.0.0
          export MEMPOOL_PAUSE_LIMIT=80000
          export SPAMMER_STATE_FILE="/home/maxim/work/projects/congested-testnet/spammer/spammer/state.json"
        '';
      };

      devShells.purs =
        let
          easy-ps = inputs.easy-purescript-nix.packages.${system};
        in
        pkgs.mkShell {
          name = "purescript-custom-shell";
          buildInputs = [
            easy-ps.purs-0_15_4
            easy-ps.spago
            easy-ps.purescript-language-server
            easy-ps.purs-tidy
            pkgs.nodejs-18_x
            pkgs.esbuild
          ];
          # ++
          # self'.devShells.ctl.buildInputs;
          shellHook = ''
            source <(spago --bash-completion-script `which spago`)
            source <(node --completion-bash)
          '';
        };

    };
}
