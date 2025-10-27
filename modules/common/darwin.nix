{
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

    # home-manager.users.${config.user} = {
    #   home.packages = with pkgs; [
    #     jetbrains.idea-ultimate
    #   ];
    # };

    ## Setting state version for system
    system.stateVersion = 6;
  };
}
