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
      harpoon = {
        enable = true;
        # IMPORTANT:
        # Remove the now-invalid `keymaps.addFile` / `keymaps.toggleQuickMenu` definitions.
      };
    };

    # Move your Harpoon keymaps to the top-level keymaps array
    keymaps = [
      {
        mode = "n";
        key = "<C-a>";
        action.__raw = "function() require'harpoon':list():add() end";
      }
      {
        mode = "n";
        key = "<C-;>";
        action.__raw = "function() require'harpoon'.ui:toggle_quick_menu(require'harpoon':list()) end";
      }
    ];

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
