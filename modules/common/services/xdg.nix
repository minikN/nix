##
## XDG configuration
##

{ config, lib, pkgs, ... }:

{
  config = {

    ## Setting up portals
    xdg.portal = {
      enable = true;
      wlr.enable = config.os.wayland;
      extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
    };

    ## Enabling XDG
    home-manager.users.${config.user} = {
      xdg.enable = true;
    };
  };
}

