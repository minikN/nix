##
## Networking configuration
##

{ config, lib, pkgs, ... }:

{
  ## DHCP needs to be enable on a per-interface basis.
  ## This is part of the machine-specific configuration.
  networking.useDHCP = false;

  ## Enable network manager
  networking.networkmanager.enable = true;
  
  ## Enabling appropriate groups
  users.users.${config.user} = {
    extraGroups = [ "networkmanager" ]; 
    isNormalUser = true;
  };
}
