#!/usr/bin/env bash

eval set -- "$TMP_OPTS"
while true; do
    case "$1" in
        --) shift; break;;
        *)
            set +e
            parse-option "$@"
            rc="$?"
            set -e
            case "$rc" in
                1|2)
                    shift $rc;;
                *)
                    exit $rc;;
            esac
    esac
done
