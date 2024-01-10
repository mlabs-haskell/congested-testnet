{ inputs, self, ... }:
{
  perSystem = { system, inputs', self', pkgs, ... }: {
    packages.prometheus-run = pkgs.writeShellApplication {
      name = "prometheus-run";
      runtimeInputs = with pkgs; [
        jq
        coreutils
        gnugrep
        websocat
        curl
        iputils
        bashInteractive
        cacert
        glibcLocales
        iproute
        socat
        utillinux
        dnsutils
        tree
      ];
      text = ''
        ln -sf ${pkgs.iana-etc}/etc/protocols /etc/protocols
        ln -sf ${pkgs.iana-etc}/etc/services /etc/services
        ${pkgs.prometheus}/bin/prometheus --config.file=${./prometheus.yaml} --storage.tsdb.path=/data
      '';
    };
  };

}
