{ config, pkgs, lib, ... }:
{
  options.vars = lib.mkOption {
    default = {
      user = "testnet";
    };
  };
}
