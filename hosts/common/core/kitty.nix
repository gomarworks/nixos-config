{ config, pkgs, ... }:

{
  programs.kitty = {
    enable = true;
    settings = {
      dynamic_background_opacity = true;  # Enable dynamic opacity
      confirm_os_window_close = 0;        # Disable close confirmation
      window_padding_width = 0;           # Set window padding to 0
      # background_opacity = "0.7";         # Set background opacity to 70%
    };
  };
}

