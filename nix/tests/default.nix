{ inputs, self, ... }:
{
  perSystem = { system, self', pkgs, ... }:
    {
      packages.tests = pkgs.writeShellApplication {
        name = "tests";
        runtimeInputs = with pkgs; [ bats ];
        text = ''
          bats ${../../tests/tests.bats}
        '';
      };
    };
}
