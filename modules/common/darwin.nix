{
  pkgs,
  globals,
  ...
}:

{
  config = {
    nix = {
      ## TODO: Create option for determinate
      ## builtins.pathExists /nix/var/nix/profiles/default/etc/profile.d/nix-installer-version
      ## is only true if determinate is used
      enable = false;

      ## Automatic garbage collection
      gc = {
        interval = {
          Hour = 3;
          Minute = 15;
          Weekday = 7;
        };
        options = "--delete-older-than 7d";
      };
    };

    ordenada.features = {
      homebrew.enable = true;
      emacs.exec-path.enable = true;
      gnupg.keychainInteraction = false;
    };

    home-manager.users.${globals.user} = {
      home.packages = with pkgs; [
        nodejs
        jetbrains.idea-ultimate
      ];
    };

    ## Setting state version for system
    system.stateVersion = 6;
  };
}
