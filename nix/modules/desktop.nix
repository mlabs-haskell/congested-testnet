{ config, lib, pkgs, ... }:

{
  services.xserver.enable = true;

  services.xserver.desktopManager.xfce.enable = true;

  services.xserver.displayManager = {
    autoLogin = {
      enable = true;
      user = "testnet"; # Replace with your username
    };
  };

  networking.networkmanager.enable = true;

  time.timeZone = "UTC";

  environment.systemPackages = with pkgs; [
    chromium
  ];

}
