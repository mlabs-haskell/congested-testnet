{ pkgs, ... }:
{
  project.name = "test";
  services.prometheus = {
    # image = "prom/prometheus:v2.43.1";
    # pots = "9090:9090";
    service.image = "crccheck/hello-world";
    service.ports = [ "80:8000" ];
  };
  # services.webserver = {
  #      image.enableRecommendedContents = true;
  #      service.useHostStore = true;
  #      service.command = [ "sh" "-c" ''
  #                  cd "$$WEB_ROOT"
  #                  ${pkgs.python3}/bin/python -m http.server 5000
  #                '' ];
  #      service.ports = [
  #        "5000:5000" # host:container
  #      ];
  #      service.environment.WEB_ROOT = "${pkgs.nix.doc}/share/doc/nix/manual";
  #      service.stop_signal = "SIGINT";
  #    };
}
