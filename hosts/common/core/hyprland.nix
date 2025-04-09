{ config, pkgs, ... }:

{
  wayland.windowManager.hyprland = {
    enable = true;

    settings = {
      "$mod" = "TAB";

      bind =
        [
          "$mod, F, exec, brave"
          ", Print, exec, grimblast copy area"
          "$mod, SPACE, exec, rofi -show drun"
        ]
        ++ (
          builtins.concatLists (builtins.genList (i:
            let ws = i + 1;
            in [
              "$mod, code:1${toString i}, workspace, ${toString ws}"
              "$mod SHIFT, code:1${toString i}, movetoworkspace, ${toString ws}"
            ]
          ) 9)
        );
    };
  };

  programs.waybar = {
    enable = true;
    settings = {
      mainBar = {
        layer = "top";
        position = "top";
        modules-left = [ "workspaces" "clock" ];
        modules-right = [ "pulseaudio" "tray" ];
        clock = {
          format = "{:%Y-%m-%d %H:%M}";
        };
      };
    };
    style = ''
      * {
        font-family: JetBrainsMono Nerd Font;
        font-size: 12px;
        color: white;
      }
      window#waybar {
        background-color: rgba(0, 0, 0, 0.6);
        border-bottom: 1px solid #444;
      }
    '';
  };

  programs.rofi = {
    enable = true;
    theme = "gruvbox-dark"; # Or pick another from `rofi-theme-selector`
  };

  xdg.enable = true;
}
