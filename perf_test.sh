#!/usr/bin/env bash
set -eux
set -o pipefail

# apt-get install r-cran-plyr r-cran-getopt r-cran-proto r-cran-ggplot2

ROOT=$(dirname $(readlink -f -- $0))

RABBITMQ_SERVER=../rabbitmq-server/scripts/rabbitmq-server
TARGET_QUEUE_PLACEMENTS=first_node,worst
TARGET_MESSAGING_PATTERNS=one_shot_direct,explicit_reply_queue,direct_reply_to
TARGET_HOST=localhost

parse-option() {
    case "$1" in
        -r|--rabbitmq-server)
            RABBITMQ_SERVER="$2"; return 2;;
        -q|--queue-placement)
            TARGET_QUEUE_PLACEMENTS="$2"; return 2;;
        -m|--messaging-pattern)
            TARGET_MESSAGING_PATTERNS="$2"; return 2;;
        -t|--target-host)
            TARGET_HOST="$2"; return 2;;
        -c|--concurrency)
            CONCURRENCY="$2"; return 2;;
        *) return Internal error; exit 1;;
    esac
}

TMP_OPTS=$(getopt -o q:m:r:t:c: --long queue-placement:messaging-pattern:rabbitmq-server:target-host:concurrency: -n perf_test -- "$@")

. $ROOT/apps/sut/priv/parse-option-loop.sh
. $ROOT/apps/sut/priv/functions.sh
. $ROOT/apps/sut/priv/perf.sh

BB_DIR=/home/binarin/mirantis-workspace/basho_bench
HEADER_DUMPED=0
SUT_NODES=(perf-test-1@$TARGET_HOST perf-test-2@$TARGET_HOST perf-test-3@$TARGET_HOST)

setup-test-nodes() {
    for-each-node "${SUT_NODES[*]}" start-fresh-background-node
    wait-nodes "${SUT_NODES[@]}"
    # join-nodes "${SUT_NODES[@]}"
}

run-benchmark() {
    echo -e "Benchmarking $ITER_VALUES"
    mkdir -p $TEST_DIR
    $BB_DIR/basho_bench --results-dir $TEST_DIR $BB_CONF
    Rscript --vanilla $BB_DIR/priv/summary.r -i $TEST_DIR/current
    cp $TEST_DIR/current/summary.png $SUMMARY_TARGET
    echo -e "$ITER_VALUES\t$($DESTRUCTIVE_ROOT/apps/sut/priv/throughput.r $TEST_DIR/current/summary.csv)" >> result.csv
}

set-benchmark-outputs() {
    TEST_DIR=./results/$QUEUE_PLACEMENT-$MESSAGING_PATTERN
    SUMMARY_TARGET=$QUEUE_PLACEMENT-$MESSAGING_PATTERN.png
}

dump-summary-header() {
    if [[ $HEADER_DUMPED -eq 0 ]]; then
        echo -e "$ITER_HEAD\ttroughput" > result.csv
        HEADER_DUMPED=1
    fi
}

partial-profiling() {
    local beam
    beam=$(find $(pwd) -name eep.beam | head -n1)
    local base
    base=${beam%.beam}
    local profile_log
    profile_log="$QUEUE_PLACEMENT-$MESSAGING_PATTERN.prof.log"
    run-ctl-for "${SUT_NODES[1]}" eval "code:load_abs(\"$base\")."
    (
        sleep 10
        echo "Taking profile sample!"
        run-ctl-for "${SUT_NODES[1]}" eval "eep:start_file_tracing(\"file_name\"), timer:sleep(10000), eep:stop_tracing()."
        echo "Profile sample taken to $profile_log"
    ) &
}

stages=(
    make
    setup-test-nodes
    iterate:QUEUE_PLACEMENT=$TARGET_QUEUE_PLACEMENTS
    iterate:MESSAGING_PATTERN=$TARGET_MESSAGING_PATTERNS
    dump-summary-header
    generate-bb-config
    set-benchmark-outputs
    partial-profiling
    run-benchmark
    run-benchmark
    run-benchmark
)
run-stages "${stages[@]}"
