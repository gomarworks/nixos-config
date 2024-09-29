{ config, lib, pkgs, ... }:

{
  programs.emacs = {
    enable = true;
    package = pkgs.emacs;
    extraPackages = epkgs: with epkgs; [
      evil
      org
      org-present
      org-modern
      org-super-agenda
      olivetti
      all-the-icons
    ];
  };

  home.file.".emacs.d/init.el".source = ./init.el;
  home.file.".emacs.d/org-agenda.el".source = ./org-agenda.el;
  home.file.".emacs.d/org-present.el".source = ./org-present.el;
}
