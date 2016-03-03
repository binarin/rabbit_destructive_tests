#!/bin/bash -eu
set -o pipefail

case "${1:?}" in
    start)
        sudo tc qdisc add dev lo root netem delay ${2:?}ms
        ;;
    stop)
        sudo tc qdisc del root dev lo || true
        ;;
    *)
        echo "Usage: $0 <start|stop>"
        exit 1
        ;;
esac
