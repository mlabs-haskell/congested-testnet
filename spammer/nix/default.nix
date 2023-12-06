inputs@{ ... }:
{
  spammer = {
    load-keys-db = import ./load-private-keys-db.nix inputs;
    psProjectFor = import ./purs-project.nix inputs;
    generate-scripts = import ./generate-scripts.nix inputs;
  };
}


