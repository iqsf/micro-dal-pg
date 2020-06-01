let
  release = import ./release.nix;
  pkgs = release.pkgs;
in pkgs.haskellPackages.shellFor {
  nativeBuildInputs = with pkgs.haskellPackages; [
    cabal-install
    ghcid
  ];
  buildInputs = with pkgs; [
                      postgresql
                      pgcli
                    ];
  packages = _: pkgs.lib.attrValues release.packages;

  shellHook = ''
        export PGDATA=./db
        export PGPORT=5435
        function psql-repl { pgcli --host /tmp -p $PGPORT localDb; }
        function cleanup {
          echo "Stopping DB..."
          pg_ctl stop
          echo "Stopping background jobs ..."
          kill 0
        }
        # https://stackoverflow.com/questions/360201/how-do-i-kill-background-processes-jobs-when-my-shell-script-exits
        trap exit INT TERM
        trap cleanup EXIT
        if [ ! -d "$PGDATA" ]; then
          LANG=C LC_ALL=C initdb
          sed -i -E -e 's/^#\s*(unix_socket_directories\s*=\s*).*$/\1'"'"'\/tmp'"'"'/' ./db/postgresql.conf
          pg_ctl -D $PGDATA -l $PGDATA.log start
          sleep 4
          createuser -p $PGPORT localDb --superuser --host /tmp
          createdb -p $PGPORT localDb --owner localDb -w --host /tmp
        else
          pg_ctl -D $PGDATA -l $PGDATA.log start
        fi
    '';

}
