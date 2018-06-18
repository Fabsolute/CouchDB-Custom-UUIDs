#!/bin/bash
erlang_version=`couch-config --erlang-version` &&
couch_version=`couch-config --couch-version` &&
lib_dir=`couch-config --erl-libs-dir` &&
config_dir=`couch-config --config-dir` &&
file_name="couch_custom_uuids-1.0.0-$erlang_version-$couch_version" &&
git pull &&
rm -rf "$file_name" "$file_name.tar.gz" &&
make plugin &&
sudo rm -rf "$lib_dir/$file_name/" &&
sudo cp -rf "$file_name" "$lib_dir" &&
sudo cp -rf deps/* "$lib_dir" &&
sudo cp -rf priv/default.d/* "$config_dir/local.d/" &&
sudo service couchdb restart &&
sudo service couchdb status
