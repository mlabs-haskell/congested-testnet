{ pkgs, config, ... }: {
  system.stateVersion = "22.11";
  environment.systemPackages = [
    pkgs.arion
    pkgs.docker-client
  ];

  virtualisation.docker.enable = false;
  virtualisation.podman.enable = true;
  virtualisation.podman.dockerSocket.enable = true;
  virtualisation.podman.defaultNetwork.settings.dns_enabled = true;

  systemd.services.check =
    {
      description = "congested-testnet";
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" "podman.service" ];
      serviceConfig =
        let
          arion-compose = pkgs.arion.build { modules = [ ./arion-compose.nix ]; inherit pkgs; };
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
          ExecStart = "${start}/bin/start";
          ExecStop = "${stop}/bin/start";
        };
    };

  users.extraUsers.testnet.extraGroups = [ "podman" ];
}
