{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/db6e089456cdddcd7e2c1d8dac37a505c797e8fa.tar.gz") {}}:

pkgs.mkShell {
  buildInputs = [
    pkgs.which
    pkgs.stack
    pkgs.perl
    pkgs.gmp
  ];

  shellHook = ''
    echo "setup stack dev environment"
    export STACK_ROOT="$PWD/.stack_global_root"
    export CABAL_DIR="$PWD/.cabal_work"
    export CABAL_CONFIG="$CABAL_DIR/config"
  '';
}
