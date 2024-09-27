{ config, pkgs, inputs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      ../common/commonConfig.nix
      inputs.home-manager.nixosModules.default
    ];

  #######################################################################
  # Bootloader Configuration homeDesktopVM
  #######################################################################
  boot.loader.grub.enable = true;
  boot.loader.grub.device = "/dev/sda";
  boot.loader.grub.configurationLimit = 5;


  #######################################################################
  # Networking Configuration
  #######################################################################
  networking.hostName = "eos"; # Define your hostname.
  networking.networkmanager.enable = true; # Enable network management
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

#  #######################################################################
#  # Nix Configuration
#  #######################################################################
#
#  nix.settings = {
#    experimental-features = [ "nix-command" "flakes" ];
#  };
#
#  # Allow installation of unfree packages
#  nixpkgs.config.allowUnfree = true;

  #######################################################################
  # Localization & Time Configuration
  #######################################################################
 
  time.timeZone = "Europe/Brussels"; # Set system timezone
  i18n.defaultLocale = "en_US.UTF-8"; # Set default locale

  #######################################################################
  # User Configuration
  #######################################################################

  users.users.gomar = {
    isNormalUser = true;
    description = "gomar";
    extraGroups = [ "networkmanager" "wheel"]; # Add user to network and admin groups
  };

  home-manager = {
    extraSpecialArgs = { inherit inputs; }; # Pass inputs to home-manager
    users.gomar = import ../common/users/gomar.nix {
      inherit inputs pkgs config;
    };
  };

  #######################################################################
  # Graphical Interface (X11 and GNOME)
  #######################################################################
  services.xserver.enable = true; # Enable X11 windowing system

  # Enable the GNOME Desktop Environment
  services.xserver.displayManager.gdm.enable = true; # Enable GNOME Display Manager
  services.xserver.desktopManager.gnome.enable = true; # Enable GNOME Desktop Environment
 
  # Delete annoying GNOME apps
  environment.gnome.excludePackages = with pkgs; [
    baobab      # disk usage analyzer
    cheese      # photo booth
    eog         # image viewer
    epiphany    # web browser
    simple-scan # document scanner
    totem       # video player
    yelp        # help viewer
    evince      # document viewer
    file-roller # archive manager
    geary       # email client
    seahorse    # password manager

    # these should be self explanatory
    gnome-calculator gnome-calendar gnome-characters gnome-clocks gnome-contacts
    gnome-font-viewer gnome-logs gnome-maps gnome-music gnome-screenshot
    gnome-system-monitor gnome-weather gnome-disk-utility pkgs.gnome-connections
  ];

  # Keyboard configuration for X11
  services.xserver.xkb = {
    layout = "us"; # Set US layout
    variant = ""; # No variant
  };

  #######################################################################
  # Hardware Configuration
  #######################################################################
  hardware.bluetooth.enable = true; # Enable Bluetooth
  hardware.pulseaudio.enable = false; # Disable PulseAudio (using pipewire) 
  security.rtkit.enable = true; # Enable real-time scheduling daemon

  # PipeWire Configuration (Sound system)
  services.pipewire = {
    enable = true;
    alsa = {
      enable = true;
      support32Bit = true; # Enable 32-bit ALSA support
    };
    pulse.enable = true; # Enable PulseAudio compatibility
    # jack.enable = true; # Uncomment for JACK support (if needed)
  };

  # Enable printing support (cups)
  services.printing.enable = true;
 
  #######################################################################
  # System-Wide Installed Packages
  #######################################################################
  environment.systemPackages = with pkgs; [
    orca-slicer
  ];

  #######################################################################
  # System wide Stylix incl. bootloader (why it isnt in HM config)
  #######################################################################

  stylix.enable = true;
  stylix.image = ../common/users/wallpaper.png;
  stylix.opacity = {
    terminal = 0.7;
  };  
  stylix.cursor.package = pkgs.apple-cursor;
  stylix.cursor.name = "macOS";
  stylix.fonts = {
    monospace = {
      package = pkgs.nerdfonts.override {fonts = ["JetBrainsMono"];};
     name = "JetBrainsMono Nerd Font Mono";
    };
    sansSerif = {
      package = pkgs.dejavu_fonts;
      name = "DejaVu Sans";
    };
    serif = {
      package = pkgs.dejavu_fonts;
      name = "DejaVu Serif";
    };
  };
  stylix.fonts.sizes = {
    desktop = 12;
    applications = 11;
    terminal = 14;
    popups = 11;
  };
 

  # services.openssh.enable = true; # Uncomment to enable OpenSSH
  # Firewall configuration (uncomment and customize if needed)
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # networking.firewall.enable = false; # Disable firewall (if not needed)

  #######################################################################
  # System Version
  #######################################################################
  system.stateVersion = "24.05"; # NixOS release version for stateful data DON'T TOUCH

}
