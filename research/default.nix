{pkgs}:
rec {
  ssh = pkgs.writeScriptBin "ssh-connect" ''
  #!/bin/sh
  ${./runssh.sh} 
  '';
}
