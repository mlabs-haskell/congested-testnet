{ repoRoot, inputs, pkgs, lib, system }:

let

  project = repoRoot.nix.project;
  ghc92 = project.variants.ghc92;

in

[
  # {
  #   inherit (project) cabalProject;
  #
  #   devShells.default = ghc92.devShell;
  #   packages = ghc92.packages;
  # }
  (
    project
  )
]
