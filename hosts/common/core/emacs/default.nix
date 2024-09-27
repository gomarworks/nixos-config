{ config, lib, pkgs, ... }:

{
  programs.emacs = {
    enable = true;
    package = pkgs.emacs;
    extraPackages = epkgs: [
      epkgs.spacious-padding
      epkgs.evil
      epkgs.org
      epkgs.org-present
    ];
  };

  home.file.".emacs.d/init.el".source = ./init.el;
}
