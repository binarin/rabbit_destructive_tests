#!/usr/bin/env bash
ROOT=$(readlink -f $(dirname $0))
. $ROOT/functions.sh

dump-default-config $ROOT
