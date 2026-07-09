{
  description = "My NixOS configurations";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-26.05";
    home-manager.url = "github:nix-community/home-manager/release-26.05";
    stylix.url = "github:danth/stylix/release-26.05";
    session-quit = {
      url = "github:ThwyIgo/session-quit";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    prismlauncher = {
      url = "github:Diegiwg/PrismLauncher-Cracked";
      #inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, home-manager, stylix, session-quit, prismlauncher }: {
    nixosConfigurations = {
      PeaceNixArch = nixpkgs.lib.nixosSystem {
        modules = [
          ({pkgs, ...}: {
            nixpkgs.overlays = [
              (final: prev: {
                session-quit = session-quit.packages.${pkgs.stdenv.hostPlatform.system}.default;
                prismlauncher = prismlauncher.packages.${pkgs.stdenv.hostPlatform.system}.prismlauncher;
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
            };
          }
        ];
      };
    };
  };
}
