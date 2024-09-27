{ config, pkgs, inputs, ... }:

{
  imports =
    [ 
      inputs.home-manager.nixosModules.default
    ];

  #######################################################################
  # Nix Configuration
  #######################################################################

  nix.settings = {
    experimental-features = [ "nix-command" "flakes" ];
  };

  # Allow installation of unfree packages
  nixpkgs.config.allowUnfree = true;

  #######################################################################
  # Localization & Time Configuration
  #######################################################################
 
  time.timeZone = "Europe/Brussels"; # Set system timezone
  i18n.defaultLocale = "en_US.UTF-8"; # Set default locale
 
  #######################################################################
  # Common System-Wide Installed Packages
  #######################################################################
  environment.systemPackages = with pkgs; [
    xdg-desktop-portal # Desktop portal for better Wayland support
    neofetch # System info tool
    asciiquarium-transparent # Fun terminal aquarium
    wget # Command line file download
    git
    lazygit # Version control system
    htop # Interactive process viewer
    nmap # Network exploration tool
    cmus # Command line music player
    vlc # Media player
  ];

}
