{ inputs, self, ... }:
{
  perSystem = { system, inputs', pkgs, ... }:
    {
      packages.generate-scripts =  
        let
          scripts  =  import ./scripts.nix { inherit pkgs;};
            # echo ${builtins.readFile script.code} > validators/always-true.ak
          aiken-action = script : ''
            ${script.code}
            aiken build
            aiken blueprint convert > script.json
            SCRIPT=$(jq '.cborHex' < script.json)
            echo "$SCRIPT" >> scripts
            echo ${toString script.count} >> counts
          '';
          compiledAll = pkgs.lib.strings.concatMapStrings aiken-action scripts; 

        in
        pkgs.writeShellApplication {
          name = "generate-scripts";
          runtimeInputs = [pkgs.coreutils inputs.aiken.packages.${system}.aiken pkgs.jq pkgs.unixtools.xxd ];
          text = ''
            WALLET=$1
            # shellcheck disable=SC2034
            VKEY=$2
            TMPDIR=$(mktemp -d)

            cd "$TMPDIR"

            # create project
            aiken new spammer/scripts
            cd scripts
            echo 'name="spammer/scripts"' > aiken.toml
            echo 'version="0.0.0"' >> aiken.toml
            ${compiledAll}
            cp scripts "$WALLET"
            mv counts "$WALLET"
          '';
        };

    };
}
