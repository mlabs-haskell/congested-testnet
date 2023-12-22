{ config, pkgs, lib, ... }:
{
  # users.users.${config.vars.user} = {
  users.users.testnet = {
    isNormalUser = true;
    createHome = true;
    home = "/home/testnet";
    description = "congested testnet";
    extraGroups = [ "wheel" ];
    password = "p";
  };
}
