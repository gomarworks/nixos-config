{ config, pkgs, inputs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      ../common/commonConfig.nix
      inputs.home-manager.nixosModules.default
    ];

  #######################################################################
  # Eos Specific Installed Packages
  #######################################################################
  environment.systemPackages = with pkgs; [
    orca-slicer
    code-cursor
    #hyprland specific packages
    waybar
    dunst
    wl-clipboard
    hyprpaper
    rofi-wayland
    libnotify
    networkmanagerapplet
  ];
  programs.steam.enable = true;
  programs.hyprland = {
    enable = true;
    withUWSM = true;
    xwayland.enable = true;
  };

  #######################################################################
  # Stylix settings for eos
  #######################################################################

  stylix.enable = true;
  stylix.image = ../common/users/wallpaper.jpg;
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
    desktop = 13;
    applications = 10;
    terminal = 12;
    popups = 14;
  };

  #######################################################################
  # Bootloader Configuration eos
  #######################################################################
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  #boot.loader.grub.enable = true;
  #boot.loader.grub.device = "/dev/sda";
  #boot.loader.grub.configurationLimit = 5;

  #######################################################################
  # Networking Configuration
  #######################################################################
  networking.hostName = "eos"; # Define your hostname.
  networking.networkmanager.enable = true; # Enable network management
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # services.openssh.enable = true; # Uncomment to enable OpenSSH
  # Firewall configuration (uncomment and customize if needed)
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # networking.firewall.enable = false; # Disable firewall (if not needed)

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
    backupFileExtension = "backup";
  };

  #######################################################################
  # Graphical Interface (X11 and GNOME)
  #######################################################################
  services.xserver.enable = true; # Enable X11 windowing system

  # Enable the GNOME Desktop Environment
  services.xserver.displayManager.gdm.enable = true; # Enable GNOME Display Manager
  services.xserver.desktopManager.gnome.enable = true; # Enable GNOME Desktop Environment

  xdg.portal = {
    enable = true;
    extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
    wlr.enable = true;
  };

  # Enable OpenGL
  hardware.graphics = {
    enable = true;
  };

  # Load nvidia driver for Xorg and Wayland
  services.xserver.videoDrivers = ["nvidia"];

  hardware.nvidia = {

    # Modesetting is required.
    modesetting.enable = true;

    # Nvidia power management. Experimental, and can cause sleep/suspend to fail.
    # Enable this if you have graphical corruption issues or application crashes after waking
    # up from sleep. This fixes it by saving the entire VRAM memory to /tmp/ instead 
    # of just the bare essentials.
    powerManagement.enable = false;

    # Fine-grained power management. Turns off GPU when not in use.
    # Experimental and only works on modern Nvidia GPUs (Turing or newer).
    powerManagement.finegrained = false;

    # Use the NVidia open source kernel module (not to be confused with the
    # independent third-party "nouveau" open source driver).
    # Support is limited to the Turing and later architectures. Full list of 
    # supported GPUs is at: 
    # https://github.com/NVIDIA/open-gpu-kernel-modules#compatible-gpus 
    # Only available from driver 515.43.04+
    open = false;

    # Enable the Nvidia settings menu,
	# accessible via `nvidia-settings`.
    nvidiaSettings = true;

    # Optionally, you may need to select the appropriate driver version for your specific GPU.
    package = config.boot.kernelPackages.nvidiaPackages.stable;
  };

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
  # System Version
  #######################################################################
  system.stateVersion = "24.05"; # NixOS release version for stateful data DON'T TOUCH

}
