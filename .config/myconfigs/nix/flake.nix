{
  description = "My NixOS configurations";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-26.05";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager/release-26.05";
    stylix.url = "github:danth/stylix/release-26.05";
      
    minimal-emacs-src = {
      url = "github:jamescherti/minimal-emacs.d";
      flake = false;
    };
    session-quit = {
      url = "github:ThwyIgo/session-quit";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    prismlauncher = {
      url = "github:Diegiwg/PrismLauncher-Cracked";
      #inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, nixpkgs-unstable, home-manager, stylix, minimal-emacs-src, session-quit, prismlauncher }: {
    nixosConfigurations = {
      PeaceNixArch = nixpkgs.lib.nixosSystem {
        modules = [
          ({pkgs, ...}: {
            nixpkgs.overlays = [
              (final: prev: {
                session-quit = session-quit.packages.${prev.stdenv.hostPlatform.system}.default;
                prismlauncher = prismlauncher.packages.${prev.stdenv.hostPlatform.system}.prismlauncher;
                antigravity-ide = (import nixpkgs-unstable {
                  system = prev.stdenv.hostPlatform.system;
                  config.allowUnfree = true;
                }).antigravity-ide;
              })
            ];
          })

          ./configuration.nix
          ./hardware-configuration.nix

          home-manager.nixosModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.extraSpecialArgs = {
              stylix = stylix.homeModules.stylix;
              inherit minimal-emacs-src;
            };
          }
        ];
      };
    };
  };
}
