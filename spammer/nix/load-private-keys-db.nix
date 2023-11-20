inputs@{...}:
let
inherit (inputs) cardano pkgs;
in
pkgs.writeShellApplication {
    name = "load-pkeys";
    runtimeInputs = [cardano];
    text = ''
    #!/bin/sh
    cardano-cli address key-gen \
        --verification-key-file "/tmp/pub.vkey" \
        --signing-key-file "/tmp/priv.skey"
    cat "/tmp/priv.skey"
    '';
}

