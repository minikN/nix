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
### Mail configuration
###
### CODE:

{ config, lib, pkgs, ... }:

{
  options = {
    mail = {
      address = lib.mkOption {
        type = lib.types.str;
        default = "";
      };

      signature = lib.mkOption {
        type = lib.types.lines;
        description = "Primary signature";
        default = '''';
      };
      
      clients = {
        thunderbird = {
          enable = lib.mkEnableOption "Enable thunderbird as a mail client";
          primary = lib.mkOption {default = ""; };
        };
        emacs = {
          enable = lib.mkEnableOption "Enable emacs as a mail client";
          primary = lib.mkOption {default = ""; };
        };
      };
    };
  };
  
  imports = [
    ./mailbox.nix
    ./work.nix

    ./clients/emacs.nix
    ./clients/thunderbird.nix
  ];

  ## General mail settings
  config = {
    home-manager.users.${config.user} = {
      accounts.email.maildirBasePath = config.const.mailDir;
      programs.mbsync.enable = true;
      programs.msmtp.enable = true;
      
      services.mbsync = {
        enable = true;
        frequency = "*:0/5";
        postExec = toString (pkgs.writeShellScript "mbsync-post" ''
## ~!shell!~
${pkgs.notmuch}/bin/notmuch new
'');
      };
      
      programs.notmuch = {
        enable = true;
        new = {
          tags = [ "new" ];
          ignore = [
            ".uidvalidity"
            ".mbsyncstate"
            ".mbsyncstate.new"
            ".mbsyncstate.journal"
          ];
        };

        hooks = let
          ## This will move messages that are in folder $1, but not tagged with $1 into
          ## the archive folder
          move-out-untagged-rule = toString (pkgs.writeShellScript "move-out-untagged-rule" ''
## ~!shell!~
for f in $(${pkgs.notmuch}/bin/notmuch search --output=files "path:/.*\/$1/ and not tag:$1" | ${pkgs.gnugrep}/bin/grep "/$1/"); do
    ${pkgs.coreutils}/bin/mv -v $f $(echo $f | ${pkgs.gnused}/bin/sed "s;/$1/;/archive/;" | ${pkgs.gnused}/bin/sed 's/,U=[0-9]*:/:/');
done
'');
          ## This will move message tagged with $1, but not in $1 folder to $1 folder.
          ## Unless message are in folder $2, then they will be ignored.
          move-in-tagged-rule = toString (pkgs.writeShellScript "move-in-tagged-rule" ''
## ~!shell!~
for f in $(${pkgs.notmuch}/bin/notmuch search --output=files "not path:/.*\/$1/ and tag:$1" | ${pkgs.gnugrep}/bin/grep -v "/''${2:-NOTHING}/"); do
    ${pkgs.coreutils}/bin/mv -v $f $(echo $f | ${pkgs.gnused}/bin/sed "s;/[[:alnum:]]*/cur/;/$1/cur/;" | ${pkgs.gnused}/bin/sed "s/,U=[0-9]*:/:/");
done
'');
          ## This will permanently remove messages tagged with 'deleted'
          remove-deleted-rule = toString (pkgs.writeShellScript "remove-deleted-rule" ''
## ~!shell!~
for f in $(${pkgs.notmuch}/bin/notmuch search --output=files tag:deleted); do
    ${pkgs.coreutils}/bin/rm -v $f;
done
'');
          ## This will initially tag every new mail with their respective mail account tag
          ## -> e.g. accounts.email.accounts.work mails with "new" tag will get tagged with "work"
          make-id-tag = let
            mail-accounts = lib.attrsets.mapAttrsToList (name: _: name) config.home-manager.users.${config.user}.accounts.email.accounts;
          in toString (pkgs.writeShellScript "make-id-tag" ''
## ~!shell!~
${lib.lists.foldl (acc: key: "${acc}${pkgs.notmuch}/bin/notmuch tag +${key} -- path:accounts/${config.mail.${key}.address}/** and tag:new\n") "" mail-accounts}
            '');

          ## Initial tagging based on folder location
          update-tags-rule = toString (pkgs.writeShellScript "update-tags-rule" ''
## ~!shell!~
${pkgs.notmuch}/bin/notmuch tag +inbox -- path:/accounts\\/.*\\/inbox/
${pkgs.notmuch}/bin/notmuch tag +draft -- path:/accounts\\/.*\\/drafts/
${pkgs.notmuch}/bin/notmuch tag +sent  -- path:/accounts\\/.*\\/sent/
${pkgs.notmuch}/bin/notmuch tag +trash -- path:/accounts\\/.*\\/trash/
${pkgs.notmuch}/bin/notmuch tag +spam  -- path:/accounts\\/.*\\/spam/
${pkgs.notmuch}/bin/notmuch tag +list  -- path:/lists\\/.*/
${pkgs.notmuch}/bin/notmuch tag +todo -inbox -sent  -- tag:inbox and tag:sent

## If file was moved out of folder on server remove respective tag
${pkgs.notmuch}/bin/notmuch tag -inbox -- not path:/accounts\\/.*\\/inbox/ and tag:inbox
${pkgs.notmuch}/bin/notmuch tag -trash -- not path:/accounts\\/.*\\/trash/ and tag:trash
${pkgs.notmuch}/bin/notmuch tag -spam  -- not path:/accounts\\/.*\\/spam/  and tag:spam
'');

          show-notmuch-notification = let
            mail-accounts = lib.attrsets.mapAttrsToList (name: _: name) config.home-manager.users.${config.user}.accounts.email.accounts;
          in toString (pkgs.writeShellScript "show-notmuch-notification" ''
## ~!shell!~
QUERY="tag:new"
ALL_QUERY="tag:new"

NEW_UNREAD=$(${pkgs.notmuch}/bin/notmuch count "$QUERY")
ALL_UNREAD=$(${pkgs.notmuch}/bin/notmuch count "$ALL_QUERY")

if [ $NEW_UNREAD -gt 0 ]; then
    ${lib.lists.foldl (acc: key: "${acc}${key}_UNREAD=$(${pkgs.notmuch}/bin/notmuch count \"tag:new AND tag:${key}\")\n") "" mail-accounts}
    read -r -d \'\' NOTIFICATION <<EOM
$NEW_UNREAD new messages
${lib.lists.foldl (acc: key: "${acc}\$${key}_UNREAD ${key}\n") "" mail-accounts}
EOM
    ${pkgs.libnotify}/bin/notify-send -t 10000 "New Mail" "$NOTIFICATION"
fi
'');  

        in {
          preNew = ''
## ~!shell!~
${move-out-untagged-rule} trash
${move-out-untagged-rule} inbox
${move-out-untagged-rule} spam

${move-in-tagged-rule} inbox archive
${remove-deleted-rule}
'';
          postNew = lib.mkOrder 500 ''
## ~!shell!~
${make-id-tag}
${update-tags-rule}

## Show notification
${show-notmuch-notification}

## Finally remove new tag
${pkgs.notmuch}/bin/notmuch tag -new -- tag:new
'';
        };
        
        search.excludeTags = [ "trash"  "spam"  "deleted" ];
      };
    };
  };
}
