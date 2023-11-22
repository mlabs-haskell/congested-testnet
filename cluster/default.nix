inputs@{ pkgs, cardano-tag, cardano, gen-testnet-config, ctl-runtime, ... }:
{
  runnet = pkgs.writeShellApplication {
    name = "runnet";
    runtimeInputs = [ pkgs.git ];
    text = ''
      #!/bin/sh

      # start (logs CONTAINER) stop
      COMMAND="$1"
      

      case $COMMAND in
          "start"|"logs"|"stop")
              ;;
          *)
              raise error "$COMMAND is not in start logs stop"
              ;;
      esac



      # add configs to start network
      if [[ "$COMMAND" == "start" ]]; then
          ${gen-testnet-config}/bin/conf
      fi

      # copy docker compose files
      TESTNET_CONFIG="$(git rev-parse --show-toplevel)/cardano-conf" 
      if [[ "$COMMAND" == "start" ]]; then
          mkdir -p "$TESTNET_CONFIG"/cluster
          for file in ${./docker}/*; do 
          cp "$file" "$TESTNET_CONFIG"/cluster/
          done

          # write .env file with variables
          echo TESTNET_CONFIG="$TESTNET_CONFIG" >> "$TESTNET_CONFIG"/cluster/.env
          echo CARDANO_TAG=${cardano-tag} >> "$TESTNET_CONFIG"/cluster/.env
          
          # start docker
          sudo systemctl start docker
      fi

      cd "$TESTNET_CONFIG"/cluster


      DOCKER_FILES=""
      for file in "$TESTNET_CONFIG"/cluster/* ; do 
        if [[ "$file" == *.yaml ]]; then
          DOCKER_FILES+="-f $(basename "$file") "
        fi
      done
      
      # shutdown containers 
      if [[ "$COMMAND" != "logs" ]]; then
        eval "sudo docker compose -p congested-testnet $DOCKER_FILES down -v"
      fi
      
      # run contaners 
      if [[ "$COMMAND" == "start" ]]; then
        eval "sudo docker compose -p congested-testnet $DOCKER_FILES up -d --remove-orphans --force-recreate --build"
      fi

      # logs 
      if [[ "$COMMAND" == "logs" ]]; then
        eval "sudo docker compose -p congested-testnet $DOCKER_FILES logs $2"
      fi
    '';
  };
}
