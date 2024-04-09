{ inputs, self, ... }:
{
  perSystem = { system, inputs', self', pkgs, ... }:
    let
      runtimeInputs = with pkgs; [
        self'.packages.cardano-node
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
        iproute2
      ];
    in
    {
      packages.update-kes = pkgs.writeShellApplication {
        name = "update-kes";
        inherit runtimeInputs;
        text = ''
          CONFIG=$1
          cd "$CONFIG"
          cardano-cli node key-gen-KES \
            --verification-key-file "pools/kes1.vkey" \
            --signing-key-file "pools/kes1.skey"


          cardano-cli node issue-op-cert \
          --kes-verification-key-file "pools/kes1.vkey" \
          --cold-signing-key-file "pools/cold1.skey" \
          --operational-certificate-issue-counter "pools/opcert1.counter" \
          --kes-period 0 \
          --out-file "pools/opcert1.cert" 
          
        '';
      };
    };
}
