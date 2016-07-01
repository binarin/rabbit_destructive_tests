#!/usr/bin/env bash
set -eu
DEBUG_PREFIX="$DEBUG_PREFIX[$(hostname)-$$]"
. $(readlink -f $(dirname $0))/functions.sh
"$@"
