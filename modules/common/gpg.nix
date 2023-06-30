##
## GnuPG configuration
##

{ config, lib, pkgs, ... }:

{
  home-manager.users.${config.user} = {
    services = {
     
     ## Enable gpg-agent with ssh support
     gpg-agent.enable = true;
     gpg-agent.enableSshSupport = true;

     ## Add SSH key
     gpg-agent.sshKeys = [ "E3FFA5A1B444A4F099E594758008C1D8845EC7C0" ];
    };

    programs = {
      gpg = {
        
        ## Enable GnuPG
        enable = true;

        ## Set homedir to `$XDG_DATA_HOME/gnupg'. This will set GNUPG_HOME.
        homedir = "${config.home-manager.users.${config.user}.xdg.dataHome}/gnupg";
      };
    };
  };
}

