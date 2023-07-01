##
## Bootloader configuration
##

{ config, lib, pkgs, ... }:

{
  options = {
    os.fonts = {
      mono = lib.mkOption {
        type = lib.types.str;
        description = "Default monospaced font";
        default = "Iosevka NFM";
      };
    };
  };

  config = {
    fonts = {
      fonts = with pkgs; [
        nerdfonts
      ];
    };
  };
}

