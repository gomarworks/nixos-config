{ config, lib, pkgs, ... }:

{
  # Simply copy the hyprland configuration file to the expected location
  home.file.".config/hypr/hyprland.conf".source = ./hyprland.conf;

  # Copy the hyprpaper configuration file to the expected location
  home.file.".config/hypr/hyprpaper.conf".source = ./hyprpaper.conf;

  # Copy the wallpaper to the expected location
  home.file.".config/hypr/wallpaper.jpg".source = ./wallpaper.jpg;
}
