{ inputs, self, ... }:
{
  perSystem = { system, self', pkgs, ... }:
    let
      psProject = (import ./functions.nix).psProjectFor pkgs;
    in
    {
      devShells.ctl = psProject.devShell;
      packages.compiled = psProject.buildPursProject { };
      packages.nodeModules = psProject.mkNodeModules { };
      # packages.ogmios = pkgs.stdenv.mkDerivation {
      #   pname = "ogmios";
      #   version = "6.8.0";
      #
      #   src = pkgs.fetchurl {
      #     url = "https://github.com/CardanoSolutions/ogmios/releases/download/v6.8.0/ogmios-v6.8.0-x86_64-linux.zip";
      #     sha256 = "sha256-cAaBfTU2LdK8NeH97WHcs7fmFVsYu2lnUI7xTD5swXE=";
      #   };
      #   nativeBuildInputs = [pkgs.unzip];
      #
      #   buildCommand = ''
      #     mkdir $out
      #     unzip $src -d $out
      #   '';
      #
      #   };
      # packages.kupo = pkgs.stdenv.mkDerivation {
      #   pname = "kupo";
      #   version = "2.9.0";
      #
      #   src = pkgs.fetchurl {
      #     url = "https://github.com/CardanoSolutions/kupo/releases/download/v2.9/kupo-v2.9.0-x86_64-linux.zip";
      #     sha256 = "sha256-+h7ofizpO3aE75WZnjFsz9C/q197LwrqtVTa0xH+V5s=";
      #   };
      #   nativeBuildInputs = [pkgs.unzip];
      #
      #   buildCommand = ''
      #     mkdir $out
      #     unzip $src -d $out
      #   '';
      #
      #   };
      # apps.purs-docs = psProject.launchSearchablePursDocs { };
    };
}
