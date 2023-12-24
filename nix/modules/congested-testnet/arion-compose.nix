{ pkgs,  ... }:
{
  project.name = "congested-testnet";
  # services.prometheus = {
  #   # image = "prom/prometheus:v2.43.1";
  #   # pots = "9090:9090";
  #   service.image = "crccheck/hello-world";
  #   service.ports = [ "80:8000" ];
  # };

  services.faucet = {
       image.enableRecommendedContents = true;
       service.useHostStore = true;
       service.command = [ "sh" "-c" ''${pkgs.congested.faucet}/bin/faucet''];
       service.ports = [
         "8000:8000" 
       ];
       # service.environment.WEB_ROOT = "${pkgs.nix.doc}/share/doc/nix/manual";
       service.stop_signal = "SIGINT";
     };
}
