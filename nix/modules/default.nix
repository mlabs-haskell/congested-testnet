{inputs, ...} : {
  imports = [./nixos-generators.nix];
  flake.nixosModules = {
      services.xserver.enable = true;
      services.xserver.displayManager.gdm.enable = true;
      services.xserver.desktopManager.gnome.enable = true;
  };
}
