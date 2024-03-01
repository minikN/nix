### NixOS Configuration
###
### Copyright © 2023 Demis Balbach <db@minikn.xyz>
###
### This file is not part of Nix/NixOS/Home Manager.
###
### My config is free software; you can redistribute it and/or modify it
### under the terms of the GNU General Public License as published by
### the Free Software Foundation; either version 3 of the License, or (at
### your option) any later version.
###
### My config is distributed in the hope that it will be useful, but
### WITHOUT ANY WARRANTY; without even the implied warranty of
### MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
### GNU General Public License for more details.
###
### You should have received a copy of the GNU General Public License
### along with my config. If not, see <http://www.gnu.org/licenses/>.
###
### COMMENT:
###
### Work mail configuration
###
### CODE:

{ config, lib, pkgs, ... }:

{
  ## General mail settings
  config = lib.mkIf config.mail.clients.thunderbird.enable {
    home-manager.users.${config.user} = {
      programs.thunderbird = {
        enable = true;
        # profiles = {
        #   work = {
        #     isDefault = true;
        #     withExternalGnupg = true;
        #   };
        # };
        settings = {
          "mail.identity.default.is_gnupg_key_id" = true;
          "mail.identity.default.last_entered_external_gnupg_key_id" = "${config.const.signingKey}";
          "mail.openpgp.fetch_pubkeys_from_gnupg" = true;
        };
      };
    };
  };
}
