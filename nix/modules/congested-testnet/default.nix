{self',...}:
{ pkgs, config, ... }: {
  system.stateVersion = "22.11";
  environment.systemPackages = [
    pkgs.arion
    pkgs.docker-client
   ( 
        let
          pkgs' = pkgs // {congested.faucet = self'.packages.faucet;};
          arion-compose = pkgs.arion.build { modules = [ ./arion-compose.nix ]; pkgs = pkgs';};
          in
          pkgs.writeShellApplication {
            name = "start";
            runtimeInputs = [ pkgs.arion pkgs.docker ];
            text = ''
              #!/bin/sh
              echo ${arion-compose}
              cat ${arion-compose}
              ${pkgs.arion}/bin/arion --prebuilt-file ${arion-compose} up -d 
              ${pkgs.arion}/bin/arion --prebuilt-file ${arion-compose} logs -f 
              '';
            }
      )
        (let
          pkgs' = pkgs // {congested.faucet = self'.packages.faucet;};
          in
        pkgs'.congested.faucet)

  
  ];

  virtualisation.docker.enable = false;
  virtualisation.podman.enable = true;
  virtualisation.podman.dockerSocket.enable = true;
  virtualisation.podman.defaultNetwork.settings.dns_enabled = true;

  systemd.services.congested-testnet =
    {
      description = "congested-testnet";
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" "podman.service" ];
      serviceConfig =
        let
          pkgs' = pkgs // {congested.faucet = self'.packages.faucet;};
          arion-compose = pkgs.arion.build { modules = [ ./arion-compose.nix ]; pkgs = pkgs';};
          start = pkgs.writeShellApplication {
            name = "start";
            runtimeInputs = [ pkgs.arion pkgs.docker ];
            text = ''
              #!/bin/sh
              ${pkgs.arion}/bin/arion --prebuilt-file ${arion-compose} up -d
            '';
          };
          stop = pkgs.writeShellApplication {
            name = "stop";
            runtimeInputs = [ pkgs.arion pkgs.docker ];
            text = ''
              #!/bin/sh
              ${pkgs.arion}/bin/arion --prebuilt-file ${arion-compose} down
            '';
          };
        in
        {
          Type = "forking";
          ExecStart = "${start}/bin/start";
          ExecStop = "${stop}/bin/stop";
        };
    };

  users.extraUsers.testnet.extraGroups = [ "podman" ];
}
# ${pkgs.arion}/bin/arion --prebuilt-file ${arion-compose} logs -f 
