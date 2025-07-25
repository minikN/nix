{
  config,
  lib,
  pkgs,
  ordanada,
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

    ## Allow unfree packages
    nixpkgs.config.allowUnfree = true;

    ## Console font
    console = {
      font = "Lat2-Terminus16";
      ## TODO: Don't hardcode this
      keyMap = "us";
    };

    ## Global packages
    ##
    ## Packages should be managed with home-manager whereever
    ## possible. Only use a set of barebones applications here.
    environment.systemPackages = with pkgs; [
      pciutils
      usbutils
    ];

    ordenada = {
      features = {
        home = {
          enable = true;
          extraGroups = [
            "video"
            "input"
          ];
          autoStartWmOnTty = "/dev/tty2";
        };

        ## SYSTEM
        networking = {
          enable = true;
          nameservers = [ "8.8.8.8" ];
        };
        pipewire.enable = true;
        fontutils = {
          enable = true;
          fonts.monospace = {
            size = 15;
            name = "Iosevka";
            package = pkgs.iosevka;
          };
        };

        ## UI
        sway = {
          enable = true;
        };
        waybar.enable = true;
        rofi.enable = true;
        # bemenu.enable = true;

        ## CORE
        password-store.enable = true;
        firefox.enable = true;
        gtk.enable = true;
        xdg.enable = true;
        bash.enable = true;
        # emacs.enable = true;
        scripts.screenshot.enable = true;
      };
    };

    home-manager.users.${config.user} = {
      home = {
        packages = with pkgs; [
          wdisplays
        ];
      };
    };

    ## Setting state version for system
    system.stateVersion = "${config.stateVersion}";
  };
}
