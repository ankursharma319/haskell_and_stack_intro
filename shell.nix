{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/30d3d79b7d3607d56546dd2a6b49e156ba0ec634.tar.gz") {}}:

pkgs.mkShell {
  buildInputs = [
    pkgs.which
    pkgs.stack
    pkgs.perl
    pkgs.gmp
    pkgs.haskellPackages.haskell-language-server
  ];

  shellHook = ''
    echo "setup localized and isolated stack,cabal,elm dev environment"
    export STACK_ROOT="$PWD/.stack_global_root"
    export CABAL_DIR="$PWD/.cabal_work"
    export CABAL_CONFIG="$CABAL_DIR/config"
    export ELM_HOME="$PWD/.elm_home"

    if [ -e $STACK_ROOT/config.yaml ]
    then
        echo "stack config already ok"
    else
        mkdir -p $STACK_ROOT
        echo "local-bin-path: \"$STACK_ROOT/_build/bin\"" > $STACK_ROOT/config.yaml
        echo "created stack global config"
    fi
    export PATH=$PATH:"$STACK_ROOT/_build/bin"
  '';
}
