{ inputs, self, ... }:
{
  perSystem = { system, inputs', pkgs, ... }:
  {
 packages.generate-scripts =
 let
  scripts = import ./scripts.nix {inherit pkgs;};
  aiken-action = script : ''
  cat ${script.code} > validators/always-true.ak
  '';
  compileAll = pkgs.lib.strings.concatMapStrings aiken-action scripts;
 in
 pkgs.stdenv.mkDerivation {
  name = "generate-scripts";
  buildInputs = [ inputs.aiken.packages.${system}.aiken  pkgs.jq  pkgs.unixtools.xxd];
  src = pkgs.runCommandNoCC "empty" {} "mkdir $out";
  buildPhase = ''
    cd $TMPDIR

    # create project
    aiken new spammer/scripts
    cd scripts
    echo 'name="spammer/scripts"' > aiken.toml
    echo 'version="0.0.0"' >> aiken.toml
    ${compileAll}
    
    aiken build
    aiken blueprint convert > script.json
    SCRIPT=$(jq '.cborHex' < script.json)
    COMPILED_CODE=$(jq '.validators[0].compiledCode' < plutus.json)
    SIZE_SCRIPT=$(($(expr length $SCRIPT)/2))

    X=$(echo $SCRIPT | awk '{print substr( $0, 6, length($0)-6)}')
    echo $X | xxd -r -p > script.flat 
    echo $SCRIPT > $out  
    echo $X >> $out  
    aiken uplc unflat script.flat >> $out  

    echo $SIZE_SCRIPT >> $out 
    aiken check 2>> $out


  '';
  };
    
};
}
