{
  config,
  pkgs,
  ...
}:

{
  config = {
    nix = {
      enable = true;

      ## Store optimization
      optimise.automatic = true;

      ## Automatic garbage collection
      gc = {
        automatic = true;
        dates = "weekly";
        options = "--delete-older-than 7d";
      };
    };

    ## Timezone and locales
    i18n.defaultLocale = "en_US.UTF-8";

    ## Global packages
    environment.systemPackages = with pkgs; [
      pciutils
      usbutils
    ];

    ## User packages
    # home-manager.users.${config.user} = {
    #   home.packages = with pkgs; [
    #     wdisplays
    #   ];
    # };

    ## Setting state version for system
    system.stateVersion = "${config.stateVersion}";
  };
}
