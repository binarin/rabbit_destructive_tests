#!/usr/bin/env bash
set -eu
set -o pipefail
export LANG=C
if ${trace:-false}; then
    set -x
fi

ROOT=$(dirname $(readlink -f $0))

RABBIT_DIR=${1:?}
NUM_NODES=${2:-3}
: ${NUM_USERS:=10}

node-numbers() {
    seq 1 ${1:?}
}

purge-node() {
    local node_number="${1:?}"

    local node_name
    node_name="$(node-name $node_number)"

    pkill -9 -f "$node_name" || true
    rm -rf "/tmp/rabbitmq-test-instances/$node_name"
}

start-node() {
    local node_number="${1:?}"
    purge-node $node_number

    # erlang.mk is not happy with nested invocations on unrelated projects
    env -i "HOME=$HOME" "PATH=$PATH" make -C "$RABBIT_DIR" \
        run-background-broker \
        RABBITMQ_PID_FILE=$(node-pid-file $node_number) \
        RABBITMQ_NODENAME=$(node-name $node_number) \
        RABBITMQ_NODE_PORT=$(node-port $node_number) \
        RABBITMQ_DIST_PORT=$(node-distribution-port $node_number)
}

node-name() {
    echo "test-cluster-node-${1:?}@localhost"
}

node-port() {
    echo $((17000 + ${1:?}))
}

node-distribution-port() {
    echo $((27000 + ${1:?}))
}

start-n-nodes() {
    local num_nodes="${1:?}"
    for node_number in $(node-numbers $num_nodes) ; do
        start-node $node_number
    done
}

join-nodes() {
    local node_to_join
    local target_node_num="${1:?}"
    shift
    for node_num_to_join in "$@"; do
        join-node $node_num_to_join $target_node_num
    done
}

join-node() {
    local node="${1:?}"
    local target="${2:?}"
    ensure-app-stopped-on-node $node
    run-ctl $node join_cluster $(node-name $target)
    start-app-on-node $node
}

ensure-app-stopped-on-node() {
    run-ctl "${1:?}" stop_app
}

start-app-on-node() {
    run-ctl "${1:?}" start_app
}

run-ctl() {
    local node_number="${1:?}"
    shift
    ERL_LIBS="${RABBIT_DIR}/deps" "${RABBIT_DIR}/scripts/rabbitmqctl" -n "$(node-name $node_number)" "$@"
}

wait-nodes() {
    local node_number
    for node_number in "$@"; do
        wait-node $node_number
    done
}

node-pid-file() {
    local node_number="${1:?}"
    echo "/tmp/$(node-name $node_number).pid"
}

wait-node() {
    local node_number="${1:?}"
    local try_no
    local await_result
    for try_no in $(seq 1 10); do
        await_result="$(run-ctl "$node_number" -t 10 eval "rabbit:await_startup().")"
        if [[ $await_result == ok ]]; then
            return 0
        fi
    done
    return 1
}

create-user() {
    local name="${1:?}"
    local password="${2:?}"
    run-ctl 1 add_user $name $password
}

create-users() {
    local num_users="${1:?}"
    local user_suffix
    for user_suffix in $(seq 1 $num_users); do
        create-user "sut$user_suffix" "sut$user_suffix"
    done
}

set-ha-policy() {
    run-ctl 1 set_policy ha-all "^ha\." '{"ha-mode":"all"}'
}

start-n-nodes $NUM_NODES
join-nodes $(node-numbers $NUM_NODES)
wait-nodes $(node-numbers $NUM_NODES)
create-users $NUM_USERS
set-ha-policy
