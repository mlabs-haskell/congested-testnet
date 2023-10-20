{pkgs}:
pkgs.writeScriptBin "runnet" ''
export ROOT=$(git rev-parse --show-toplevel)
${./runnet.sh}
''
