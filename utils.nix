{ pkgs ? import <nixpkgs> {}, lib ? import <nixpkgs/lib> {}, config, ... }:

{
  ## Write a trivial emacs package from arguments and add it to the load path.
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
    extraPackages = epkgs: packages ++ [ pkg ];
    extraConfig = ''
${if require then "(require '${pkg.pname})" else ""}
${config}
    '';
  };

  ## Utility functions for handling outputs.
  outputs = {
    ## Maps the list of outputs to a specific attribute.
    mapAttr = {left ? true, attr ? "name", outputs ? config.os.output.configs }: let
      mappedOutputs = lib.lists.imap0 (i: output: let
        target = if left then output.left else output.right;
      in "${target.${attr}}");
    in mappedOutputs outputs;
  };
}
