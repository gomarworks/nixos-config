{ config, lib, pkgs, ... }:

{
  programs.emacs = {
    enable = true;
    package = pkgs.emacs;
    extraPackages = epkgs: with epkgs; [
      evil
      doom-modeline
      org
      org-present
      org-modern
      olivetti
      all-the-icons
    ];
  };

  home.file = {
    ".emacs.d/init.el".source = ./init.el;
    ".emacs.d/org-agenda.el".source = ./org-agenda.el;
    ".emacs.d/org-present.el".source = ./org-present.el;
    ".emacs.d/org-mode.el".source = ./org-mode.el;
    ".emacs.d/keybindings.el".source = ./keybindings.el;
  };
}
