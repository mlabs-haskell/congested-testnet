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
          pkgs.rust-analyzer
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
          nodePackages.prettier
          nodejs
          ogmios
          kupo
          python311Packages.fire
        ] 
        ++
        self'.devShells.ctl.buildInputs
        # ++
        # (with pkgs.python310Packages; [ jupyterlab scikit-learn pandas psycopg2 matplotlib tabulate ])
        ;
        shellHook = ''
          export SSHOPTS="-p 2222"
          export OGMIOS_URL="0.0.0.0"
          export OGMIOS_PORT="1337"
          export KUPO_URL="0.0.0.0"
          export KUPO_PORT="1442"
          export NSPAMMERS=1
          export SPAMMER_METRIC_PORT="8001"
          export FAUCET_PORT="8000"
          export FAUCET_LOVELACE_AMOUNT="1000000000000"
          export SPAMMER_STATE_FILE="/home/maxim/work/projects/congested-testnet/spammer/spammer/state.json"
          # export walletPath="/home/maxim/work/projects/congested-testnet/containers/genesis_spo_data/wallet.skey";
          export WALLET_SKEY_PATH="/home/maxim/work/projects/congested-testnet/containers/genesis_spo_data/wallet.skey";
          export N_WORKERS=1
          export MEMPOOL_PAUSE_LIMIT=80000
          export MEMPOOL_UNPAUSE_LIMIT=60000
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
