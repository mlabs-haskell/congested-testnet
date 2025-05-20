{ inputs, self, ... }:
{
  perSystem = { system, self', pkgs, ... }:
    {
      packages.tests = pkgs.writeShellApplication {
        name = "tests";
        runtimeInputs = with pkgs; [ bats ];
        # Disable shellcheck for this script
        checkPhase = "";
        text = ''
          #!/usr/bin/env bash
          set -e
          
          # Set environment variables
          export MEMPOOL_PAUSE_LIMIT=200000
          export SLOT_LENGTH=1
          export MAX_BLOCK_BODY_SIZE=65000
          export SPAMMER_ON=true
          export FAUCET_ON=true
          
          # Function to clean up containers
          function cleanup {
            echo "Cleaning up containers..."
            docker-compose --profile genesis_spo down -v || true
          }
          
          # Ensure cleanup happens on exit
          trap cleanup EXIT
          
          echo "Starting containers..."
          docker-compose --profile genesis_spo up -d
          
          echo "Waiting for services to initialize..."
          sleep 15
          
          echo "Running tests..."
          bats ${../tests/tests.bats}
          TEST_EXIT_CODE=$?
          
          echo "Tests completed with exit code: $TEST_EXIT_CODE"
          exit $TEST_EXIT_CODE
        '';
      };
    };
}
