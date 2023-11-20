inputs@{ ... }:
let
  inherit (inputs) cardano pkgs;
in
pkgs.writeShellApplication {
  name = "load-pkeys";
  runtimeInputs = [ cardano pkgs.jq ];
  text = ''
    #!/bin/sh
    SQL_COMMAND="" 
    for _ in $(seq 1 100); do 
      cardano-cli address key-gen \
          --verification-key-file "/tmp/pub.vkey" \
          --signing-key-file "/tmp/priv.skey"
      PKEY=$(jq '.cborHex' < "/tmp/priv.skey")
      PUBKEY=$(jq '.cborHex' < "/tmp/pub.vkey")
      SQL_COMMAND+="INSERT INTO pkeys (pkey, pubkey) VALUES ('$PKEY', '$PUBKEY');" 
    done

    DB_NAME="spammer"
    DB_USER="user"
    DB_HOST="localhost"
    DB_PORT="5432"


    # Execute the SQL file
    psql -h "$DB_HOST" -p "$DB_PORT" -U "$DB_USER" -d "$DB_NAME" -c "$SQL_COMMAND"

  '';
}

