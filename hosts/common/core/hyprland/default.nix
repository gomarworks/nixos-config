{ config, lib, pkgs, ... }:

{
  # Simply copy the hyprland configuration file to the expected location
  home.file.".config/hypr/hyprland.conf".source = ./hyprland.conf;
}
