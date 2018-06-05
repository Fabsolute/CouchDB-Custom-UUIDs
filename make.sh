#!/bin/bash
git pull &&
rm -rf couch_custom_uuids-1.0.0-18-1.6.1 couch_custom_uuids-1.0.0-18-1.6.1.tar.gz &&
make plugin &&
rm -rf /usr/lib/x86_64-linux-gnu/couchdb/erlang/lib/couch_custom_uuids-1.0.0-18-1.6.1/ &&
cp -rf couch_custom_uuids-1.0.0-18-1.6.1 /usr/lib/x86_64-linux-gnu/couchdb/erlang/lib &&
service couchdb restart &&
service couchdb status