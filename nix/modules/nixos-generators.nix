{ flake, self, inputs, ...}:{ 
  perSystem = { system, pkgs, ... }:
  let generators = inputs.nixos-generators;
  in
    {
     packages.vm = generators.nixosGenerate {
       inherit system;
        modules = [
         ./test.nix
        ];
        format = "vm";
      };
    };

}
