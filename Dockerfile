# Base image
FROM nixos/nix:latest AS builder

ENV NIX_CONFIG="experimental-features = nix-command flakes"
COPY ./flake.nix  /tmp/build/
COPY ./flake.lock  /tmp/build/
COPY ./nix  /tmp/build/
COPY ./scripts /tmp/build/

WORKDIR /tmp/build
RUN nix build .#hello

