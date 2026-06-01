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

    zed-netcoredbg-src = {
      url = "github:qwadrox/zed-netcoredbg";
      flake = false;
    };
  };
  outputs = inputs@{ self, nixpkgs, home-manager, impermanence, winapps, zed-extensions, ... }: {
    nixosConfigurations.Thiago-ambulante = nixpkgs.lib.nixosSystem {
      modules = [
        {
          nixpkgs.overlays = [
            zed-extensions.overlays.default
            (final: prev: {
              zed-extensions = prev.zed-extensions // {
                netcoredbg = prev.buildZedRustExtension {
                  name = "netcoredbg";
                  version = "1.0.0";
                  src = inputs.zed-netcoredbg-src;
                  cargoPatches = [ ./patches/zed-netcoredbg-Cargo.lock.patch ];
                  cargoHash = "sha256-wgDzJRj2gqrckyFr72gXxulLwzCxhJ2aXEnlPggCZQ8=";

                  meta = with prev.lib; {
                    description = "NetCoreDbg extension for Zed";
                    license = licenses.mit;
                  };
                };
              };
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
          home-manager.users.thiago = ./home-manager/thiago.nix;
          home-manager.sharedModules = [
            zed-extensions.homeManagerModules.default
          ];
        }
      ];
    };
  };
}
