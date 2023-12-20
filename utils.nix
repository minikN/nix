{ pkgs, ... }:

{
  emacsPkg = {name, description, code, config ? "", packages ? [], require ? false}: let
    pkg = pkgs.emacsPackages.trivialBuild {
      pname = name;
      version = "1.0";
      src = pkgs.writeText "${name}.el" ''
;;; ${name}.el --- ${description} -*- lexical-binding: t -*-

${code}

(provide '${name})
    '';
    };
  in rec {
    extraPackages = epkgs: [ pkg ] ++ packages;
    #extraConfig = if load then "(require '${pkg.pname})" else config;
    extraConfig = ''
${if require then "(require '${pkg.pname})" else ""}
${config}
    '';
  };
}
