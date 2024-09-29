{ config, lib, pkgs, ... }:

{
  programs.emacs = {
    enable = true;
    package = pkgs.emacs;
    extraPackages = epkgs: with epkgs; [
      spacious-padding
      visual-fill-column
      evil
      org
      org-present
      org-bullets
      org-modern
    ];
  };

  home.file.".emacs.d/init.el".source = ./init.el;
}
