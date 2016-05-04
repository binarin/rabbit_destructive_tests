#!/usr/bin/env bash
set -exu
set -o pipefail

ROOT=$(dirname $(readlink -f -- $0))

. $ROOT/apps/sut/priv/functions.sh

BB_DIR=/home/binarin/mirantis-workspace/basho_bench
CONCURRENCY=1
DURATION=1

# export PATH=/home/binarin/mirantis-workspace/otp/bin:$PATH

setup-test-nodes() {
    start-fresh-background-node ssl-1
    start-fresh-background-node ssl-2
    start-fresh-background-node ssl-3

    wait-nodes ssl-1 ssl-2 ssl-3
    join-nodes ssl-1 ssl-2 ssl-3
}

test-description() {
    echo "erl=$ERLANG_SSL,amqp=$AMQP_SSL,ha=$HA"
}

amqp-port-under-test() {
    local node_name="${1:?}"
    local base_port; base_port=$(base-port-for $node_name)
    if [[ $AMQP_SSL -gt 0 ]]; then
        echo $(amqp-ssl-port $base_port)
    else
        echo $(amqp-port $base_port)
    fi
}

generate-bb-config() {
    BB_CONF=/tmp/bb.conf.tmp
    rm -rf $BB_CONF
    
    set-opt mode max
    set-opt concurrent $CONCURRENCY
    set-opt duration $DURATION
    set-opt operations '[{rpc, 100}]'
    set-opt driver bb_rpc_driver
    set-opt code_paths "[$(echo $(pwd)/*/*/ebin | perl -lanE 'say join ",", map { qq{"$_"} } @F')]"
    set-opt amqp_servers "$(cat <<EOF
[{"127.0.0.1", $(amqp-port-under-test ssl-1)}
,{"127.0.0.1", $(amqp-port-under-test ssl-2)}
,{"127.0.0.1", $(amqp-port-under-test ssl-3)}
]
EOF
)"

    set-opt amqp_ssl_options "$(ssl-options)"
}

ssl-options() {
    local ssl_ca_dir; ssl_ca_dir=$(node-dir ssl-certificates)
    if [[ $AMQP_SSL -gt 0 ]]; then
        cat <<EOF
[{cacertfile, "$ssl_ca_dir/cacert.pem"}
,{certfile, "$ssl_ca_dir/client/cert.pem"}
,{keyfile, "$ssl_ca_dir/client/key.pem"}
,{verify, verify_peer}
,{fail_if_no_peer_cert,true}
,{ciphers, "DHE-RSA-AES256-SHA256:DHE-DSS-AES256-SHA256:DHE-RSA-AES128-SHA256:DHE-DSS-AES128-SHA256:AES128-SHA256:DHE-RSA-AES256-SHA:DHE-DSS-AES256-SHA:AES256-SHA:DHE-RSA-AES128-SHA:DHE-DSS-AES128-SHA:AES128-SHA" }
]
EOF
    else
        echo "none";
    fi
}


set-opt() {
    while [[ $# -gt 0 ]]; do
        echo "{${1:?}, ${2:?}}." >> $BB_CONF
        shift 2
    done
}

run-benchmark() {
    local test_dir="${1:?}"
    mkdir -p $test_dir
    $BB_DIR/basho_bench --results-dir $test_dir $BB_CONF
    Rscript --vanilla $BB_DIR/priv/summary.r -i $test_dir/current
    cp $test_dir/current/summary.png result-A${AMQP_SSL}E${ERLANG_SSL}H${HA}.png
}

BASELINE=
make
for ERLANG_SSL in 0 1; do
    setup-test-nodes
    for HA in 0; do
        if [[ $HA -gt 0 ]]; then
            run-ctl -n ssl-1@localhost set_policy ha-all "." '{"ha-mode":"all"}'
        fi
        for AMQP_SSL in 0 1; do
            TEST="results/$(date +'%Y%m%d%H%M%S')-$(test-description)"
            generate-bb-config
            run-benchmark $TEST
            if [[ -z $BASELINE ]]; then
                BASELINE=$TEST
            else
                Rscript --vanilla $BB_DIR/priv/compare.r \
                        -o cmp$AMQP_SSL$ERLANG_SSL$HA.png \
                        --dir1 $BASELINE/current/ --tag1 base \
                        --dir2 $TEST/current/ --tag2 "A=$AMQP_SSL,E=$ERLANG_SSL,H=$HA"
            fi
        done
    done
done
