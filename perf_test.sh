#!/usr/bin/env bash
set -eu
set -o pipefail

ROOT=$(dirname $(readlink -f -- $0))

BB_DIR=/home/binarin/mirantis-workspace/basho_bench
. $ROOT/apps/sut/priv/functions.sh
. $ROOT/apps/sut/priv/perf.sh

HEADER_DUMPED=0


# export PATH=/home/binarin/mirantis-workspace/otp/bin:$PATH

setup-test-nodes() {
    start-fresh-background-node ssl-1
    start-fresh-background-node ssl-2
    start-fresh-background-node ssl-3

    wait-nodes ssl-1 ssl-2 ssl-3
    join-nodes ssl-1 ssl-2 ssl-3
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

run-stages() {
    local ITER_HEAD_ORIG="${ITER_HEAD:-}"
    local ITER_VALUES_ORIG="${ITER_VALUES:-}"
    if [[ $# -eq 0 ]]; then
        return 0
    fi
    local this_stage="$1"; shift
    case $this_stage in
        iterate:*)
            if [[ $this_stage =~ ^iterate:([A-Z0-9_]+)=(.+)$ ]]; then
                local var_name="${BASH_REMATCH[1]}"
                local values="${BASH_REMATCH[2]}"
                local cur_value
                while IFS=, read cur_value; do
                    eval $var_name="$cur_value"
                    echo "Setting $var_name to $cur_value"
                    local ITER_HEAD="${ITER_HEAD_ORIG:-}${ITER_HEAD_ORIG:+\t}$var_name"
                    local ITER_VALUES="${ITER_VALUES_ORIG:-}${ITER_VALUES_ORIG:+\t}$cur_value"
                    run-stages "$@"
                done < <(echo $values | sed -e 's/,/\n/g') # write it this way to prevent subshell
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

stages=(
    make
    setup-test-nodes
    iterate:QUEUE_PLACEMENT=first_node,worst
    iterate:MESSAGING_PATTERN=explicit_reply_queue,direct_reply_to
    dump-summary-header
    generate-bb-config
    set-benchmark-outputs
    run-benchmark
)

run-stages "${stages[@]}"
