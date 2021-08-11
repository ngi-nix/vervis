#!/bin/sh

VERVIS='https://dev.angeley.es/s/fr33domlover/r'

DEPS='dvara
      hit-graph
      hit-harder
      hit-network
      darcs-lights
      darcs-rev
      http-client-signature
      http-signature
      ssh
      persistent-graph
      persistent-migration
      persistent-email-address
      time-interval-aeson
      yesod-http-signature
      yesod-mail-send'

mkdir -p lib
cd lib
for dep in $DEPS; do
    if [ -d "$dep" ]; then
        darcs pull --repodir="$dep" --all
    else
        darcs clone "$VERVIS/$dep"
    fi
done
