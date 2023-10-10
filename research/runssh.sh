#!/bin/sh
sudo rm -rf /tmp/sql-socket
mkdir /tmp/sql-socket
ssh -L /tmp/sql-socket/.s.PGSQL.5432:/run/postgresql/.s.PGSQL.5432 -N cardano-cli@node-1.mainnet.ctl-runtime.staging.mlabs.city

