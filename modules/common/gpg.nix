##
## GnuPG configuration
##

{ config, lib, pkgs, ... }:

{
  home-manager.users.${config.user} = {
    services = {
     gpg-agent.enable = true;
     gpg-agent.enableSshSupport = true;
    };

    programs = {
      gpg = {
        enable = true;
        #homeDir = "${config.xdg.dataHome}/gnupg";
        #homeDir = "${con/gnupg";
      };
    };
  };
}

