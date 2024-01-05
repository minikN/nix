# This file has been generated by node2nix 1.11.1. Do not edit!

{nodeEnv, fetchurl, fetchgit, nix-gitignore, stdenv, lib, globalBuildInputs ? []}:

let
  sources = {};
in
{
  typescript-language-server = nodeEnv.buildNodePackage {
    name = "typescript-language-server";
    packageName = "typescript-language-server";
    version = "4.2.0";
    src = fetchurl {
      url = "https://registry.npmjs.org/typescript-language-server/-/typescript-language-server-4.2.0.tgz";
      sha512 = "1yKDqKeWLTQkN4mN+CT84aBr7ckp6sNVb8DZg+eXl0TDl14edn6Yh1wPqPA1rQ4AGVJc02fYbXTFsklaVYy4Uw==";
    };
    buildInputs = globalBuildInputs;
    meta = {
      description = "Language Server Protocol (LSP) implementation for TypeScript using tsserver";
      license = "Apache-2.0";
    };
    production = true;
    bypassCache = true;
    reconstructLock = true;
  };
  typescript = nodeEnv.buildNodePackage {
    name = "typescript";
    packageName = "typescript";
    version = "5.3.3";
    src = fetchurl {
      url = "https://registry.npmjs.org/typescript/-/typescript-5.3.3.tgz";
      sha512 = "pXWcraxM0uxAS+tN0AG/BF2TyqmHO014Z070UsJ+pFvYuRSq8KH8DmWpnbXe0pEPDHXZV3FcAbJkijJ5oNEnWw==";
    };
    buildInputs = globalBuildInputs;
    meta = {
      description = "TypeScript is a language for application scale JavaScript development";
      homepage = "https://www.typescriptlang.org/";
      license = "Apache-2.0";
    };
    production = true;
    bypassCache = true;
    reconstructLock = true;
  };
}
