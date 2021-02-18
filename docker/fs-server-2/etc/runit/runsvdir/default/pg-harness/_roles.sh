#!/bin/sh

# Don't try to create roles if already created. This guards against
# restarts of the main server.
if [ -f $HOME/.roles-created ]; then
  exit 0
fi

echo "Creating roles..."
psql -o /dev/null <<EOF
CREATE ROLE "${PG_HARNESS_ADMIN_USER}" ENCRYPTED PASSWORD '${PG_HARNESS_ADMIN_PASS}' SUPERUSER CREATEDB CREATEROLE INHERIT LOGIN;
CREATE ROLE "${PG_HARNESS_CLIENT_USER}" ENCRYPTED PASSWORD '${PG_HARNESS_CLIENT_PASS}' INHERIT LOGIN;
EOF

touch $HOME/.roles-created
