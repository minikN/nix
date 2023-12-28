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
    extraPackages = epkgs: [ pkg ] ++ packages;
    extraConfig = ''
${if require then "(require '${pkg.pname})" else ""}
${config}
    '';
  };

  ## Utility functions for handling outputs.
  outputs = {

    ## Concat the output names for left or right of a specific output configuration.
    ## Will return a concatenated string of all outputs that are either left or right.
    concat = {left ? true, outputs ? config.os.output.configs }: let
      concatenatedOutputs = lib.lists.foldl (all: output: let
        target = if left then output.left else output.right;
      in
        if builtins.stringLength all > 0 then "${all}\" \"${target.name}" else "${target.name}"
      ) "";
    in concatenatedOutputs outputs;

    ## Maps the list of outputs to a specific attribute.
    mapAttr = {left ? true, attr ? "name", outputs ? config.os.output.configs }: let
      mappedOutputs = lib.lists.imap0 (i: output: let
        target = if left then output.left else output.right;
      in "${target.${attr}}");
    in mappedOutputs outputs;
  };
}
