inputs@{ pkgs, aiken, system, ... }:
pkgs.writeShellApplication {
  name = "generate-scripts";
  runtimeInputs = [ aiken.outputs.packages.${system}.aiken ];
  text = ''
    ROOT="/tmp"
    cd "$ROOT"
    [ ! -d "$ROOT/aiken" ] && echo "create aiken project in temp" && aiken new spammer/aiken
    cd "aiken"

    function true_seq {
      local n=$1
      local RES="True"
      for ((i=1; i<=n; i++)); do
        RES+=" && True"
      done
      echo "$RES"
    }
    
    for i in $(seq 1000 1020); do
      cat > "validators/script.ak" <<- EOF
      validator {
        fn always_true(_datum : Data, _redeemer : Data, _context : Data ) {
        $(true_seq "$i")
        }
      }
    EOF
      aiken build
      aiken blueprint convert > "script.json"
      CBORHEX="$(jq '.cborHex' < "script.json")"
      echo "$CBORHEX" >> "$ROOT/scripts.txt"  
    done

    # remove double quotes
    sed -i 's/"//g' "$ROOT/scripts.txt"

  '';
}
