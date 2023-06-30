##
## Git configuration
##

{ config, lib, pkgs, ... }:

{
  home-manager.users.${config.user} = {

    programs = {
      ## Enable git
      git.enable = true;
      
      ## Set username and email according to predefined options
      git.userName = "${config.fullName}";
      git.userEmail = "${config.primaryMail}";

      ## Set up signing key and auto-sign commits
      git.signing.key = "F17DDB98CC3C405C";
      git.signing.signByDefault = true;

      ## Extra config
      git.extraConfig = {
        pull = {
          rebase = false;
        };
      };
    };
  };
}

