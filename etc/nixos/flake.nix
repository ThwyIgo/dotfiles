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

    minimal-emacs-src = {
      url = "github:jamescherti/minimal-emacs.d";
      flake = false;
    };
    luciole = {
      url = "github:noctalia-dev/luciole";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    keycloak-portal-theme = {
      url = ./config/keycloak/themes;
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs = inputs@{ self, nixpkgs, home-manager, impermanence, keycloak-portal-theme, ... }: {
    nixosConfigurations.Thiago-ambulante = nixpkgs.lib.nixosSystem {
      specialArgs = {
      	inherit keycloak-portal-theme;
      };

      modules = [
        impermanence.nixosModules.impermanence
        ./configuration.nix
        home-manager.nixosModules.home-manager
        {
          home-manager.useGlobalPkgs = true;
          home-manager.useUserPackages = true;
          home-manager.extraSpecialArgs = {
            inherit (inputs) minimal-emacs-src;
          };
          home-manager.sharedModules = [ inputs.luciole.homeModules.default ];
          home-manager.users.thiago = ./home-manager/thiago.nix;
        }
        inputs.luciole.nixosModules.default
      ];
    };
  };
}
