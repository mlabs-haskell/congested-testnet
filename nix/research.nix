{ inputs, self, ... }:
{
  perSystem = { system, pkgs, ... }: {
    packages.ssh = pkgs.writeScriptBin "ssh-connect" ''
      #!/bin/sh
      ${../research/runssh.sh} 
    '';
  };
}
