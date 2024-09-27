{ config, pkgs, ... }:

{
  programs.nixvim = {
    enable = true;
    defaultEditor = true;

    # Plugin configuration
    plugins = {
      lualine.enable = true;        # Status line plugin
      lazygit.enable = true;
      noice.enable = true;
      transparent.enable = true;    # Transparent background
      web-devicons.enable = true;
      telescope = {                 # Fuzzy finder plugin
        enable = true;
        keymaps."<C-p>" = "git_files";  # Ctrl-P for git files search
      };
      harpoon = {                   # Bookmarking plugin
        enable = true;
        keymaps.addFile = "<C-a>";         # Add file shortcut
        keymaps.toggleQuickMenu = "<C-;>"; # Toggle quick menu shortcut
      };
    };

    # Clipboard and other options
    clipboard = {
      register = "unnamedplus";           # Use system clipboard
      providers.wl-copy.enable = true;    # Enable clipboard on Wayland
    };

    opts = {
      relativenumber = true;  # Relative line numbers
      incsearch = true;       # Incremental search
    };
  };
}

