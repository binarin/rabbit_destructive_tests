#!/usr/bin/env bash
: ${MESSAGING_PATTERN:=explicit_reply_queue}
: ${QUEUE_PLACEMENT:=worst}
: ${CONCURRENCY:=100}
: ${DURATION:=1}

: ${BB_DIR:=$(readlink -f "${DESTRUCTIVE_ROOT}/deps/basho_bench")}
: ${BB_CONF:=/tmp/bb.conf.tmp.$$}

set-bb-opt() {
    while [[ $# -gt 0 ]]; do
        echo "{${1:?}, ${2:?}}." >> $BB_CONF
        shift 2
    done
}

generate-bb-config() {
    rm -rf $BB_CONF
    
    set-bb-opt mode max
    set-bb-opt concurrent $CONCURRENCY
    set-bb-opt duration $DURATION
    set-bb-opt operations '[{rpc, 100}]'
    set-bb-opt driver bb_rpc_driver
    set-bb-opt code_paths "[$(echo $(pwd)/*/*/ebin | perl -lanE 'say join ",", map { qq{"$_"} } @F')]"
    set-bb-opt amqp_servers "$(cat <<EOF
[{"127.0.0.1", $(choose-default-amqp-port ssl-1)}
,{"127.0.0.1", $(choose-default-amqp-port ssl-2)}
,{"127.0.0.1", $(choose-default-amqp-port ssl-3)}
]
EOF
)"
    set-bb-opt oslo_queue_placement "${QUEUE_PLACEMENT}"
    set-bb-opt amqp_ssl_options "$(bb-ssl-options)"
}

bb-ssl-options() {
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
