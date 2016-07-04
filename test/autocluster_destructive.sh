#!/usr/bin/env bash
set -eu
set -o pipefail

ROOT=$(readlink -f $(dirname $0)/..)

RABBIT=https://github.com/binarin/rabbitmq-server
RABBIT_REF=gh/rabbitmq-server-fix-join-cluster-when-already-member
RABBIT_DIR=$ROOT/tmp/autocluster-destructive-rabbit

AUTOCLUSTER=https://github.com/binarin/rabbitmq-autocluster
AUTOCLUSTER_REF=gh/rabbitmq-autocluster-detect-join-errors
AUTOCLUSTER_DIR=$ROOT/tmp/autocluster-destructive-autocluster

export RABBITMQ_SERVER=$RABBIT_DIR/scripts/rabbitmq-server

. $ROOT/apps/sut/priv/functions.sh

# time we wait before assuming that destructive operation was fully propagated
# should be greater than net_ticktime
SLEEP_AFTER_OP=${SLEEP_AFTER_OP:-10}

SUT_NODES=()
NUM_NODES=${NUM_NODES:-10}
for node in $(seq 1 $NUM_NODES) ; do
    SUT_NODES+=("rabbit@docker-autocluster-$node")
done

UNUSED_NODES=("${SUT_NODES[@]}")
declare -A NODE_STATUS

run-destructive-test() {
    local op
    local action_counter=1
    while true; do
        op=$(random-node-operation)
        echo "##################################################"
        echo "#### Performing operation number $action_counter - $op"
        echo "##################################################"
        action_counter=$(($action_counter + 1))
        perform-operation $op
        sleep $SLEEP_AFTER_OP
        if ! wait-for-settle-down || ! health-check; then
            dump-diagnostics
            return 1
        fi
    done
}

dump-diagnostics() {
    :
}

run-with-timeout() {
    local timeout="${1:?}"
    shift
    (
        "$@" &
        child_pid=$!
        trap -- "" SIGTERM
        (
            sleep $timeout
            kill $child_pid 2> /dev/null
        ) &
        wait $child_pid
    )
}

health-check() {
    local running_nodes; running_nodes=$(nodes-with-status running)
    local test_node
    local err=0
    for test_node in $running_nodes; do
        if ! run-with-timeout 20 run-ctl-for $test_node node_health_check; then
            err=1
        fi
    done
    return $err
}

set-node-status() {
    NODE_STATUS["${1:?}"]="${2:?}"
}

nodes-with-status() {
    local expected_status="${1:?}"
    local node
    local status
    for node in ${!NODE_STATUS[@]}; do
        status=${NODE_STATUS[$node]}
        if [[ $status == $expected_status ]]; then
            echo $node
        fi
    done
}

wait-for-settle-down() {
    echo "== Waiting for cluster to settle down"
    local running_nodes; running_nodes=$(nodes-with-status running)
    local try_no
    local test_node
    local failed_result
    local attempts=21
    for try_no in $(seq 1 $attempts); do
        echo "== Waiting for cluster to settle down, attempt $try_no of $attempts"
        failed_result=
        for test_node in $running_nodes; do
            echo "== Inspecting $test_node view of the cluster"
            if ! assert-running-nodes-for $test_node "$running_nodes"; then
                failed_result=true
            fi
        done
        if [[ -z $failed_result ]]; then
            echo "== All nodes agree on cluster membership"
            return 0
        fi
        echo "== Sleeping before next check of cluster settle down"
        sleep $((SLEEP_AFTER_OP / 7))
    done
    echo "== Cluster failed to settle down"
    return 1
}

assert-running-nodes-for() {
    local node="${1:?}"
    local space_separated_nodes="${2:?}"
    local assertion_result
    assertion_result=$(run-ctl-for $node eval "ExpectedNodes = lists:sort([list_to_atom(N) || N <- string:tokens(\"$space_separated_nodes\", \"\\n \")]), RunningNodes = lists:sort(rabbit_mnesia:cluster_nodes(running)), case RunningNodes of ExpectedNodes -> true; _ -> {expected, ExpectedNodes, got, RunningNodes} end.")
    case $assertion_result in
        true)
            return 0;;
        *)
            echo "Failed assertion $assertion_result"
            return 1;;
    esac
}

available-operations() {
    available-new-node-operations
    available-stopped-node-operations
    available-running-node-operations
    available-connected-node-operations
    available-disconnected-node-operations
}

available-new-node-operations() {
    local node_name
    if ! node-limit-reached; then
        for node_name in $(unused-node-names); do
            echo "ro-start-new:$node_name"
        done
    fi
}

available-stopped-node-operations() {
    :
}

available-running-node-operations() {
    local node
    for node in $(nodes-with-status running); do
        echo "ro-refresh:$node"
        echo "ro-restart:$node"
    done
}

available-connected-node-operations() {
    :
}

available-disconnected-node-operations() {
    :
}

perform-operation() {
    local operation="${1:?}"
    local parts
    IFS=':' read -ra parts <<< "$operation"
    local operation=${parts[0]}
    local node_name=${parts[1]}
    $operation $node_name
}

unused-node-names() {
    echo "${UNUSED_NODES[@]}"
}

node-limit-reached() {
    [[ ${#UNUSED_NODES[@]} -eq 0 ]]
}

random-node-operation() {
    local operations; operations=($(available-operations))
    if [[ ${#operations[@]} -eq 0 ]]; then
        echo "No operations available, bailing out"
        exit 1
    fi
    local position=$(($RANDOM % ${#operations[@]}))
    echo ${operations[$position]}
}

ro-restart() {
    local node_name="${1:?}"
    echo "Restarting $node_name"
    kill-rabbit-proc $node_name
    start-background-node $node_name
    wait-node $node_name
}

ro-refresh() {
    local node_name="${1:?}"
    echo "Forcefeully refreshing $node_name"
    start-fresh-autocluster-node $node_name
}

ro-start-new() {
    local node_name="${1:?}"
    mark-node-used $node_name
    echo "Starting fresh $node_name"
    start-fresh-autocluster-node $node_name
    set-node-status $node_name running
}

mark-node-used() {
    local node_name="${1:?}"
    local unused_node
    local new_unused=
    for unused_node in "${UNUSED_NODES[@]}"; do
        if [[ $unused_node != $node_name ]]; then
            new_unused+=($unused_node)
        fi
    done
    UNUSED_NODES=("${new_unused[@]}")
}

start-fresh-autocluster-node() {
    local node="${1:?}"
    local cleanup_interval=10
    reset-node $node
    run-plugins-for $node --offline enable rabbitmq_management autocluster > /dev/null
    set-config-for $node \
                   rabbit cluster_partition_handling pause_minority \
                   rabbit log_levels "[{autocluster, debug}, {connection, info}]" \
                   autocluster backend etcd \
                   autocluster autocluster_failure stop \
                   autocluster cleanup_interval $cleanup_interval \
                   autocluster cluster_cleanup true \
                   autocluster cleanup_warn_only false \
                   autocluster etcd_scheme http \
                   autocluster etcd_host '"autocluster-eval-etcd"' \
                   autocluster etcd_port 4001

    # Several attempts to start - we should give some time for cluster cleanup/recovery
    local try_no
    for try_no in $(seq 1 3); do
        echo "== Starting fresh node $node"
        start-background-node $node
        echo "== Give $node some time to start or fail"
        sleep 20
        echo "== Waiting for $node"
        if wait-node $node; then
            return 0
        fi
        echo "== Sleeping before next start attempt of $node"
        sleep $cleanup_interval
    done
    echo "== Rabbit $node failed to start"
    return 1
}

remove-old-docker-containers() {
    set +e
    docker ps | grep -P 'docker-autocluster-\d+' | awk '{print $1}' |xargs -l1 docker rm -f > /dev/null 2>&1
    docker rm -f autocluster-eval-etcd > /dev/null 2>&1
    set -e
}

ensure-etcd-container() {
    ensure-docker-network
    ensure-running-docker-container \
        autocluster-eval-etcd $(ensure-docker-image microbox/etcd:latest) \
        --name autocluster-eval-etcd --bind-addr=0.0.0.0 --addr=autocluster-eval-etcd:4001
}

checkout-and-build-autocluster() {
    if [[ ! -d $AUTOCLUSTER_DIR ]]; then
        git clone https://github.com/aweber/rabbitmq-autocluster $AUTOCLUSTER_DIR
        (
            cd $AUTOCLUSTER_DIR
            git remote add gh $AUTOCLUSTER
            git remote update gh
            git checkout -b some-junk
            git branch -D master
            git checkout -b master $AUTOCLUSTER_REF
        )
    fi
    make -C $AUTOCLUSTER_DIR dist current_rmq_ref=stable base_rmq_ref=stable
}

checkout-and-build-rabbit() {
    if [[ ! -d $RABBIT_DIR ]]; then
        git clone https://github.com/rabbitmq/rabbitmq-server $RABBIT_DIR
        (
            cd $RABBIT_DIR
            git remote add gh $RABBIT
            git remote update gh
            git checkout -b stable $RABBIT_REF
        )
    fi
    make -C $RABBIT_DIR dist PLUGINS=rabbitmq_management current_rmq_ref=stable base_rmq_ref=stable
}

inject-autocluster-into-rabbit() {
    cp $AUTOCLUSTER_DIR/plugins/autocluster*.ez $AUTOCLUSTER_DIR/plugins/rabbitmq_aws*.ez $RABBIT_DIR/plugins
}

setup() {
    checkout-and-build-autocluster
    checkout-and-build-rabbit
    inject-autocluster-into-rabbit
    remove-old-docker-containers
    ensure-etcd-container
}

run-unjoined-partitions-test() {
    # subset of SUT_NODES
    local my_nodes="rabbit@docker-autocluster-1 rabbit@docker-autocluster-2 rabbit@docker-autocluster-3"

    # we need iptables inside docker
    local DOCKER_OPTS=--privileged

    # form a 3-node cluster
    for-each-node "$my_nodes" ro-start-new
    wait-for-settle-down
    for-each-node "$my_nodes" set-node-status disconnected

    # allow TCP communication only within this 3-node cluster
    wall-off-nodes $my_nodes

    # wait for etcd TTL
    sleep 60

    # start second 3-node cluster
    local other_nodes="rabbit@docker-autocluster-4 rabbit@docker-autocluster-5 rabbit@docker-autocluster-6"
    for-each-node "$other_nodes" ro-start-new

    # do cluster integrity check, but only on the new set of nodes
    wait-for-settle-down

    # reconnect first set of nodes with the rest of the world
    for-each-node "$my_nodes" remove-wall
    for-each-node "$my_nodes" set-node-status running

    # and in the end we should have 6-node healthy cluster
    wait-for-settle-down
}

wall-off-nodes() {
    local comment=destructive_partition_a32cdcd99559185800e68e2e00f6f1d4
    local nodes="$@"
    local target_ip
    local node_ip
    local target
    local node
    for node in $nodes; do
        node-iptables-flush-comment $node $comment
        node-iptables-ensure-empty-chain $node destructive_partition_input
        node-iptables-ensure-empty-chain $node destructive_partition_output
        node-iptables-c $node $comment -I INPUT -p tcp '!' -i lo -j destructive_partition_input
        node-iptables-c $node $comment -I OUTPUT -p tcp '!' -o lo -j destructive_partition_input

        for target in "$nodes"; do
            target_ip=$(node-docker-container-ip $target)
            node_ip=$(node-docker-container-ip $node)
            docker exec $(host-part $node) iptables -A destructive_partition_input -p tcp -s $target_ip -j ACCEPT
            docker exec $(host-part $node) iptables -A destructive_partition_output -p tcp -d $target_ip -j ACCEPT
        done

        node-iptables -A destructive_partition_output -j DROP
        node-iptables -A destructive_partition_input -j DROP
    done

}

case "$1" in
    test)
        setup
        run-destructive-test
        ;;
    unjoined-partitions)
        setup
        run-unjoined-partitions-test
        ;;
    *)
        "$@"
        ;;
esac
