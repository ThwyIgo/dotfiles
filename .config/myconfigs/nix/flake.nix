{
  description = "My NixOS configurations";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.11";
    home-manager.url = "github:nix-community/home-manager/release-25.11";
    stylix.url = "github:danth/stylix/release-25.11";
    session-quit = {
      url = "github:ThwyIgo/session-quit";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, home-manager, stylix, session-quit }: {
    nixosConfigurations = {
      PeaceNixArch = nixpkgs.lib.nixosSystem {
        modules = [
          {
            nixpkgs.overlays = [ (final: prev: {
              session-quit = session-quit.packages.${prev.stdenv.hostPlatform.system}.default;
            }) ];
          }

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
