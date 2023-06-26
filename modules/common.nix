{ config, pkgs, home, ... }:

{
  #imports = [ <home-manager/nixos> ];

  services.dbus.enable = true;
  xdg.portal = {
    enable = true;
    wlr.enable = true;
    extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
  };

  hardware = {
    opengl.enable = true;
    opengl.driSupport = true;
  };

  home-manager.users.db = { pkgs, ... }: {
    home.stateVersion = "23.05";
    home.packages = [ pkgs.emacs pkgs.htop ];

    wayland.windowManager.sway = {
      enable = true;
      xwayland = true;
      config = {
        floating.border = 10;
      };
      systemd.enable = true;
      wrapperFeatures = {
        base = true;
        gtk = true;
      };
      extraSessionCommands = "
	export SDL_VIDEODRIVER=wayland
        export QT_QPA_PLATFORM=wayland
        export QT_WAYLAND_DISABLE_WINDOWDECORATIONS=1
        export _JAVA_AWT_WM_NOPARENTING=1
      ";
    };
  };
}

