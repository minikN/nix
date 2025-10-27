{
  lib,
  pkgs,
  globals,
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

    ordenada = {
      features = {
        home.enable = true;
        userInfo = {
          username = "${globals.user}";
          fullName = "${globals.fullName}";
          email = "${globals.email}";
          gpgPrimaryKey = "${globals.gpgKey}";
        };
      };
    };
 };
}
