# Copyright 2024 Shea Levy
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

{
  description = "An event-oriented observability library";

  inputs = {
    haskell-nix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskell-nix/nixpkgs-unstable";
    pre-commit-hooks-nix.url = "github:cachix/pre-commit-hooks.nix";
  };

  outputs =
    inputs@{ flake-parts, haskell-nix, nixpkgs, pre-commit-hooks-nix, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } ({ lib, ... }: {
      imports = [ pre-commit-hooks-nix.flakeModule ];

      systems = [ "x86_64-linux" "x86_64-darwin" "aarch64-darwin" ];

      perSystem = { system, config, pkgs, ... }:
        let
          project = pkgs.haskell-nix.cabalProject' {
            src = ./.;
            compiler-nix-name = "ghc981";
            shell.tools = {
              cabal = { };
              fourmolu = { };
            };
          };

          flake = project.flake { };
        in {
          inherit (flake) checks packages apps;

          devShells = flake.devShells // {
            default = lib.overrideDerivation flake.devShells.default (orig: {
              shellHook = (orig.shellHook or "") + ''
                ${config.pre-commit.installationScript}
              '';
            });
          };

          pre-commit.settings.hooks = {
            nixfmt.enable = true;
            fourmolu.enable = true;
            hlint.enable = true;
            deadnix.enable = true;
            statix.enable = true;
          };

          _module.args.pkgs = import nixpkgs {
            inherit system;
            overlays = [ haskell-nix.overlay ];
            inherit (haskell-nix) config;
          };
        };
    });

  nixConfig = {
    extra-substituters = [ "https://cache.iog.io" ];
    extra-trusted-public-keys =
      [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
    allow-import-from-derivation = "true";
  };
}
