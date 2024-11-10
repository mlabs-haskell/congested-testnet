{ inputs, self, ... }:
{
  perSystem = { system, inputs', pkgs, ... }:
    {
      packages.generate-scripts1 = 
        pkgs.writeShellApplication {
          name = "generate-scripts";
          runtimeInputs = [ pkgs.python311 inputs.aiken.packages.${system}.aiken];
          text = ''
           wallet_path=$1
           echo "$wallet_path"
           cd "$wallet_path"
           python ${./generate_scripts.py}
           # aiken new spammer/scripts
           # cd scripts
           # echo 'name="spammer/scripts"' > aiken.toml
           # aiken build
           # aiken blueprint convert > script.json
          '';
          };

      packages.generate-scripts =
        let
          scripts = import ./scripts.nix { inherit pkgs; };
          aiken-action = script:
            if script.code == ""
            then ''
              echo "" >> "../../scripts"
              echo ${toString script.count} >> "../../counts"

            ''
            else ''
              ${script.code}
              aiken build
              aiken blueprint convert > script.json
              SCRIPT=$(jq '.cborHex' < script.json)
              SCRIPT=$(echo "$SCRIPT" | tr -d '"')
              echo "$SCRIPT" >> "../../scripts"
              echo ${toString script.count} >> "../../counts"
            '';
          compiledAll = pkgs.lib.strings.concatMapStrings aiken-action scripts;

        in
        pkgs.writeShellApplication {
          name = "generate-scripts";
          runtimeInputs = [ pkgs.coreutils inputs.aiken.packages.${system}.aiken pkgs.jq pkgs.unixtools.xxd ];
          text = ''
            WALLET=$1
            # shellcheck disable=SC2034
            VKEY=$2

            # Remove both single and double quotes from VKEY
            VKEY=$(echo "$VKEY" | tr -d '"')


            TMPDIR=$(mktemp -d "$WALLET/tmp.XXXXXX")

            cd "$TMPDIR"

            # create project
            aiken new spammer/scripts
            cd scripts
            echo 'name="spammer/scripts"' > aiken.toml
            echo 'version="0.0.0"' >> aiken.toml ${compiledAll}
            cd ../.. 
            rm -rf tmp*
          '';
        };

    };
}
