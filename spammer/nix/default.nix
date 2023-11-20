inputs@{ ... }:
let
  inherit (inputs) cardano pkgs;
in
{
  load-keys-db = import ./load-private-keys-db.nix inputs;
  psProjectFor = import ./purs-project.nix inputs;
}


