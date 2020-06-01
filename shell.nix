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
        export PGHOST=/tmp
        export PGPORT=5435
        export DBNAME=localDb
        export DBUSER=localDb
        function psql-repl { pgcli --host $PGHOST -p $PGPORT $DBNAME; }
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
          sleep 3
          createuser $DBUSER --superuser --host $PGHOST -p $PGPORT
          createdb $DBNAME --owner $DBUSER -w --host $PGHOST -p $PGPORT
        else
          pg_ctl -D $PGDATA -l $PGDATA.log start
        fi

        export TEST_PG_DBHOST=$PGHOST
        export TEST_PG_DBPORT=$PGPORT
        export TEST_PG_DBUSER=$DBUSER
        export TEST_PG_DBPASSWORD=""
        export TEST_PG_DBNAME=$DBNAME

    '';

}
