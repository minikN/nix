{
  lib,
  pkgs,
  globals,
  inputs,
  ...
}:

{
  options =
    let
      mkConst = const: (lib.mkOption { default = const; });
    in
    {
      user = mkConst globals.user;
      fullName = mkConst globals.fullName;
      email = mkConst globals.email;
      gpgKey = mkConst globals.gpgKey;
      stateVersion = mkConst globals.stateVersion;
    };
  config = {
    nix = {
      ## Enabling flakes
      extraOptions = ''
        experimental-features = nix-command flakes
        warn-dirty = false
      '';
    };

    ## Timezone and locales
    time.timeZone = "Europe/Berlin";

    ## Allow unfree packages
    nixpkgs.config.allowUnfree = true;

    ## Global packages
    environment.systemPackages = with pkgs; [
      git
      vim
      wget
    ];

    home-manager.users."${globals.user}".imports = [
      inputs.ordenada.homeModules.ordenada
      {
        home.stateVersion = globals.stateVersion;
      }
    ];

    ordenada = {
      features = {
        userInfo = {
          enable = true;
          username = "${globals.user}";
          fullName = "${globals.fullName}";
          email = "${globals.email}";
          gpgPrimaryKey = "${globals.gpgKey}";
          extraGroups = [ "wheel" ];
        };

        home = {
          enable = true;
          primaryUser = true;
          autoStartWmOnTty = "/dev/tty1";
        };

        bash.enable = true;
        alacritty.enable = true;
        ghostty.enable = true;

        networking.enable = true;
        keyboard.enable = true;
        fontutils = {
          enable = true;
          fonts.monospace = {
            size = 15;
            name = "Iosevka Nerd Font Mono";
            package = pkgs.nerd-fonts.iosevka;
          };
          fonts.unicode = {
            name = "Noto Color Emoji";
            package = pkgs.noto-fonts-color-emoji;
            size = 11;
          };
        };

        gnupg = {
          enable = true;
          sshKeys = [ "E3FFA5A1B444A4F099E594758008C1D8845EC7C0" ];
        };
        git = {
          enable = true;
          signCommits = true;
          signingKey = globals.gpgKey;
        };

        docker = {
          enable = true;
          colimaFlags = {
            cpu = 4;
            memory = 8;
          };
        };
        firefox.enable = true;

        nix = {
          enable = true;
          enablePolymode = true;
        };
        json.enable = true;
        yaml.enable = true;
        javascript.enable = true;
        compile.enable = true;
        android = {
          enable = true;
          allowUnfreeAndAcceptLicenses = true;
          activeSdkVersion = "36";
          sdks = [
            { platformVersion = "36"; }
          ];
          emulators = [
            {
              persistentData = true;
              platformVersion = "36";
              extraConfig = {
                "hw.lcd.width" = "800";
                "hw.lcd.density" = "300";
                "hw.lcd.height" = "1624";
                "hw.ramSize" = "4096";
              };
            }
          ];
        };

        emacs = {
          enable = true;
          advancedUser = true;

          #exec-path.enable = true;

          appearance.enable = true;
          all-the-icons.enable = true;
          completion.enable = true;
          consult.enable = true;
          corfu = {
            enable = true;
            autoShow = false;
          };
          hideshow.enable = true;
          dired.enable = true;
          embark.enable = true;
          evil.enable = true;
          help.enable = true;
          keymaps.enable = true;
          marginalia.enable = true;
          modus-themes.enable = true;
          orderless.enable = true;

          #rainbow-delimiters.enable = true;
          #eglot.enable = true;
          #flymake.enable = true;

          project.enable = true;
          shell.enable = true;
          vertico.enable = true;
          vterm.enable = true;
          which-key.enable = true;
        };

        xdg.enable = true;
        password-store.enable = true;
      };
    };
  };
}
