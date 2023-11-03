{ repoRoot, inputs, pkgs, lib, system }:

let

  project = repoRoot.nix.project;
  ghc92 = project.variants.ghc92;

in

[
  # (
  #   let inherit (project) cabalProject;
  #   in
  #   cabalProject.shellFor 
  #
  # )
  (
    project
  )
]
