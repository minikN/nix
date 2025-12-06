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
          (import ../modules/common/darwin.nix {
            inherit
              inputs
              globals
              lib
              pkgs
              ;
          })
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
