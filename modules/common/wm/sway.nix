##
## Sway configuration
##

{ config, lib, pkgs, ... }:

{
  ## Setting appropriate options so other modules can use them.
  os.wayland = true;
  os.wm = "sway";

  ## Sway
  home-manager.users.${config.user} = {
    wayland.windowManager.sway = {
      enable = true;
      systemd.enable = true;
      wrapperFeatures = {
        base = true;
        gtk = true;
      };

      extraSessionCommands = ''
        export SDL_VIDEODRIVER=wayland
        export QT_QPA_PLATFORM=wayland
        export QT_WAYLAND_DISABLE_WINDOWDECORATIONS=1
        export _JAVA_AWT_WM_NOPARENTING=1
      '';
    };
  };
}

