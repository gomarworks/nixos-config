{
  description = "GomNix multi machine magic flake";

  inputs = {
    
    ##########################################################
    # NixOS and HomeManager package sources
    ##########################################################

    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";

    home-manager = {
      url = "github:nix-community/home-manager/release-24.11";
      #inputs.nixpkgs.follows = "nixpkgs";
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
      url = "github:danth/stylix/release-24.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    
    # Hyprland input for better plugin support
    hyprland.url = "github:hyprwm/Hyprland";
    hyprland-plugins = {
      url = "github:hyprwm/hyprland-plugins";
      inputs.hyprland.follows = "hyprland"; # Prevents version mismatch.
    };
  };

  outputs = { self, nixpkgs, nixpkgs-unstable, nixos-hardware, ... }@inputs: {
    nixosConfigurations = {

      # EOS - Home Desktop configuration
      eos = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        specialArgs = { inherit inputs; };
        modules = [
          ./hosts/eos/configuration.nix 
          inputs.home-manager.nixosModules.default
          inputs.stylix.nixosModules.stylix
        ];
      };

      # FREEDMAN - Laptop configuration
      freedman = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        specialArgs = { inherit inputs; };
        modules = [
          ./hosts/freedman/configuration.nix 
          inputs.home-manager.nixosModules.default
          inputs.stylix.nixosModules.stylix
        ];
      };

      # ATLAS - Work desktop configuration
      atlas = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        specialArgs = { inherit inputs; };
        modules = [
          ./hosts/atlas/configuration.nix 
          inputs.home-manager.nixosModules.default
          inputs.stylix.nixosModules.stylix
        ];
      };

      # MINIMISES - mini pc
      minimises = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        specialArgs = { inherit inputs; };
        modules = [
          ./hosts/minimises/configuration.nix 
          inputs.home-manager.nixosModules.default
          inputs.stylix.nixosModules.stylix
        ];
      };
    };
  };
}
