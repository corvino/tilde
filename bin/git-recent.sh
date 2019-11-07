#! /usr/bin/env bash

git for-each-ref --sort=committerdate refs/remotes | tail -r -n ${1:-10} | awk -F ' ' '{print $1 "  " $3}'
