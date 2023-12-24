{ self', ... }:
{ lib, config, ... }:
{

  environment.systemPackages = [
    self'.packages.faucet
  ];
}
