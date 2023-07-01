##
## Services configuration
##
## Services that should be used on every system.

{ config, lib, pkgs, ... }:

{
  config = {

    ## DBus
    services.dbus.enable = true;
  };
}

