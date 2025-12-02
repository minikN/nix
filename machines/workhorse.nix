{
  inputs,
  globals,
  ...
}:

inputs.darwin.lib.darwinSystem {
  system = "aarch64-darwin";
  modules = [
    inputs.home-manager.darwinModules.home-manager
    inputs.ordenada.darwinModules.ordenada
    (
      { pkgs, lib, ... }:
      {
        imports = [
          (import ../modules/common {
            inherit
              inputs
              globals
              lib
              pkgs
              ;
          })
          ../modules/common/darwin.nix
        ];
        # system.stateVersion = 6;
        # ordenada.features = {
        #   home.enable = true;
        #   emacs.enable = true;
        # };

        # home-manager.users.${globals.user} = {
        #   home = {
        #     packages = with pkgs; [ ];
        #   };
        # };
      }
    )
  ];
}
