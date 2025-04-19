{
  description = "My NixOS configurations";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.11";
    # nixpkgs.follows = "nixos-cosmic/nixpkgs-stable";
    # nixos-cosmic.url = "github:lilyinstarlight/nixos-cosmic";
    home-manager.url = "github:nix-community/home-manager/release-24.11";
    stylix.url = "github:danth/stylix/release-24.11";
  };

  outputs = { self, nixpkgs, home-manager, stylix }: {
    nixosConfigurations = {
      PeaceNixArch = nixpkgs.lib.nixosSystem {
        modules = [
          ./configuration.nix
          ./hardware-configuration.nix
          
          # {
          #   nix.settings = {
          #     substituters = [ "https://cosmic.cachix.org/" ];
          #     trusted-public-keys = [ "cosmic.cachix.org-1:Dya9IyXD4xdBehWjrkPv6rtxpmMdRel02smYzA85dPE=" ];
          #   };
          # }
          # nixos-cosmic.nixosModules.default
          
          home-manager.nixosModules.home-manager
          {
            home-manager.extraSpecialArgs = {
              stylix = stylix.homeManagerModules.stylix;
            };
          }
        ];
      };
    };
  };
}
