{
  description = "GomNix multi machine magic flake";

  inputs = {
    
    ##########################################################
    # NixOS and HomeManager package sources
    ##########################################################

    #nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11;

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    }; 
    
    ##########################################################
    # Utilities
    ##########################################################
    
    disko = {
      url = "github:nix-community/disko";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixos-hardware = {
      url = "github:NixOS/nixos-hardware";
    };

    nixvim = {
      url = "github:nix-community/nixvim";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Stylix input for additional configurations
    stylix = {
      url = "github:danth/stylix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, nixos-hardware, ... }@inputs: {
    nixosConfigurations = {

      # EOS - Home Desktop configuration
      eos = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        specialArgs = { inherit inputs; };
        modules = [
          ./hosts/eos/configuration.nix # Home desktop VM's configuration
          inputs.home-manager.nixosModules.default
          inputs.stylix.nixosModules.stylix
        ];
      };

      # FREEDMAN - Laptop configuration
      freedman = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        specialArgs = { inherit inputs; };
        modules = [
          ./hosts/freedman/configuration.nix # Home desktop VM's configuration
          inputs.home-manager.nixosModules.default
          inputs.stylix.nixosModules.stylix
        ];
      };

      # NYX - Home Server configuration
      nyx = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        specialArgs = { inherit inputs; };
        modules = [
          ./hosts/nyx/configuration.nix # Home desktop VM's configuration
          inputs.home-manager.nixosModules.default
          inputs.stylix.nixosModules.stylix
        ];
      };

      # ATLAS - Work desktop configuration
      atlas = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        specialArgs = { inherit inputs; };
        modules = [
          ./hosts/atlas/configuration.nix #  desktop VM's configuration
          inputs.home-manager.nixosModules.default
          inputs.stylix.nixosModules.stylix
        ];
      };

      # MiniMises - mini pc
      minimises = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        specialArgs = { inherit inputs; };
        modules = [
          ./hosts/minimises/configuration.nix #  desktop VM's configuration
          inputs.home-manager.nixosModules.default
          inputs.stylix.nixosModules.stylix
        ];
      };
    };
  };
}
