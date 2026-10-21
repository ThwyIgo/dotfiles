{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    impermanence = {
      url = "github:nix-community/impermanence";
      inputs.nixpkgs.follows = "";
      inputs.home-manager.follows = "";
    };
    winapps = {
      url = "github:winapps-org/winapps";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    zed-extensions = {
      url = "github:DuskSystems/nix-zed-extensions";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    minimal-emacs-src = {
      url = "github:jamescherti/minimal-emacs.d";
      flake = false;
    };

    keycloak-portal-theme = {
      url = ./config/keycloak/themes;
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs = inputs@{ self, nixpkgs, home-manager, impermanence, winapps, zed-extensions, keycloak-portal-theme, ... }: {
    nixosConfigurations.Thiago-ambulante = nixpkgs.lib.nixosSystem {
      specialArgs = {
      	inherit keycloak-portal-theme;
      };

      modules = [
        {
          nixpkgs.overlays = [
            zed-extensions.overlays.default
            (final: prev: {
              winapps = winapps.packages."${prev.stdenv.hostPlatform.system}".winapps;
              winapps-launcher = winapps.packages."${prev.stdenv.hostPlatform.system}".winapps-launcher;
            })
          ];
        }

        impermanence.nixosModules.impermanence
        ./configuration.nix
        home-manager.nixosModules.home-manager
        {
          home-manager.useGlobalPkgs = true;
          home-manager.useUserPackages = true;
          home-manager.extraSpecialArgs = {
            inherit (inputs) minimal-emacs-src;
          };
          home-manager.users.thiago = ./home-manager/thiago.nix;
          home-manager.sharedModules = [
            zed-extensions.homeManagerModules.default
          ];
        }
      ];
    };
  };
}
