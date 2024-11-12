{ inputs, self, ... }:
{
  perSystem = { system, inputs', pkgs, ... }:
    {
      packages.generate-scripts = 
        pkgs.writeShellApplication {
          name = "generate-scripts";
          runtimeInputs = [ pkgs.python311 inputs.aiken.packages.${system}.aiken];
          text = ''
           dir="$(pwd)"
           rm -rf "$TMPDIR"
           mkdir -p "$TMPDIR"
           cd "$TMPDIR"
           python ${./generate_scripts.py} > "$dir/Scripts.js"
           cp "$TMPDIR/scripts/plutus.json" "$dir/plutus.json"
          '';
          };

    };
}
