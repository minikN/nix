{
  config,
  pkgs,
  ...
}:

{
  imports = [
    ./system/boot.nix
    ./system/filesystem.nix
    ./system/filesystem/nas.nix
  ];

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

    ordenada.features = {
      sway = {
        enable = true;
        extraKeybindings = { ... }: {};
      };
      waybar.enable = true;
      rofi.enable = true;
      pipewire.enable = true;
      networking.enable = true;
    };

    ## Setting state version for system
    system.stateVersion = "${config.stateVersion}";
  };
}
