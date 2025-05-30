{ config, lib, pkgs, ... }:

{
  programs.emacs = {
    enable = true;
    package = pkgs.emacs;
    extraPackages = epkgs: with epkgs; [
      evil
      doom-modeline
      hide-mode-line
      org
      org-present
      org-modern
      org-roam
      olivetti
      visual-fill-column
      all-the-icons
      vertico
    ];
  };

  home.file = {
    ".emacs.d/init.el".source = ./init.el;
    ".emacs.d/org-agenda.el".source = ./org-agenda.el;
    ".emacs.d/org-present.el".source = ./org-present.el;
    ".emacs.d/org-mode.el".source = ./org-mode.el;
    ".emacs.d/keybindings.el".source = ./keybindings.el;
    ".emacs.d/invoice.css".source = ./invoice.css;
    
    # MWL system files
    ".emacs.d/mwl-admin/init.el".source = ./mwl-admin/init.el;
    ".emacs.d/mwl-admin/pricing.el".source = ./mwl-admin/pricing.el;
    ".emacs.d/mwl-admin/export.el".source = ./mwl-admin/export.el;
    ".emacs.d/mwl-admin/invoice.css".source = ./mwl-admin/invoice.css;
    
    # MWL capture templates
    ".emacs.d/mwl-admin/capture/init.el".source = ./mwl-admin/capture/init.el;
    ".emacs.d/mwl-admin/capture/3dprint.el".source = ./mwl-admin/capture/3dprint.el;
    ".emacs.d/mwl-admin/capture/consulting.el".source = ./mwl-admin/capture/consulting.el;
    ".emacs.d/mwl-admin/capture/invoice.el".source = ./mwl-admin/capture/invoice.el;
  };
}
