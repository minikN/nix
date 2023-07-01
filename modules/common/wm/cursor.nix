##
## Cursor configuration
##

{ config, lib, pkgs, ... }:

{
  home-manager.users.${config.user} = {

    ## Enabling bigger cursor
    home.pointerCursor = {
      name = "Adwaita";
      package = pkgs.gnome.adwaita-icon-theme;
      size = 24;
      x11 = {
        enable = true;
        defaultCursor = "Adwaita";
      };
    };
  };
}

