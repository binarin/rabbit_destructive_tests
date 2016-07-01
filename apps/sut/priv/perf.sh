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

bb-amqp-credentials() {
    local node_name="${1:?}"
    echo "{\"$(host-part $node_name)\", $(choose-default-amqp-port $node_name)}"
}

all-nodes-bb-amqp-credentials() {
    echo "["
    local node_name
    local non_first_iteration=
    for node_name in "${SUT_NODES[@]}"; do
        if [[ -n $non_first_iteration ]]; then
            echo -n ", "
        fi
        non_first_iteration=1
        bb-amqp-credentials $node_name
    done
    echo "]"
}

generate-bb-config() {
    rm -rf $BB_CONF

    set-bb-opt log_level info
    set-bb-opt mode max
    set-bb-opt concurrent $CONCURRENCY
    set-bb-opt duration $DURATION
    set-bb-opt operations '[{rpc, 100}]'
    set-bb-opt driver bb_rpc_driver
    set-bb-opt code_paths "[$(echo $(pwd)/*/*/ebin | perl -lanE 'say join ",", map { qq{"$_"} } @F')]"
    set-bb-opt amqp_servers "$(all-nodes-bb-amqp-credentials)"
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

run-stages() {
    local cur_value
    local var_name
    local values
    local ITER_HEAD_ORIG="${ITER_HEAD:-}"
    local ITER_VALUES_ORIG="${ITER_VALUES:-}"
    if [[ $# -eq 0 ]]; then
        return 0
    fi
    local this_stage="$1"; shift
    case $this_stage in
        iterate:*)
            if [[ $this_stage =~ ^iterate:([A-Z0-9_]+)=(.+)$ ]]; then
                var_name="${BASH_REMATCH[1]}"
                values="${BASH_REMATCH[2]}"
                for cur_value in $(echo $values | tr "," "\n"); do
                    eval $var_name="$cur_value"
                    echo "Setting $var_name to $cur_value"
                    local ITER_HEAD="${ITER_HEAD_ORIG:-}${ITER_HEAD_ORIG:+\t}$var_name"
                    local ITER_VALUES="${ITER_VALUES_ORIG:-}${ITER_VALUES_ORIG:+\t}$cur_value"
                    run-stages "$@"
                done
            else
                echo "Malformed iteration stage: $this_stage"
            fi
            ;;
        *)
            echo Running $this_stage
            $this_stage
            run-stages "$@"
            ;;
    esac
}
