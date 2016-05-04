let
  pkgs = import <nixpkgs> {};
  stdenv = pkgs.stdenv;
in with pkgs; {
  bbEnv = stdenv.mkDerivation rec {
    name = "basho_bench";
    version = "1.0";
    src = null;
    buildInputs = [
      (rWrapper.override { packages = with pkgs.rPackages; [ plyr getopt proto ggplot2 ]; })
      erlangR18
      # (stdenv.lib.overrideDerivation pkgs.erlangR18 (oldAttrs: rec {
      #                            name = "erlang-18.3";
      #                            version = "18.3";
      #                            src = pkgs.fetchurl {
      #                                url = "http://www.erlang.org/download/otp_src_${version}.tar.gz";
      #                                sha256 = "1hy9slq9gjvwdb504dmvp6rax90isnky6chqkyq5v4ybl4lq3azx";
      #                            };
      #                            postInstall = "";
      #                          }))
    ];
  };
}
