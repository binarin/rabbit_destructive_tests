RABBITMQ_SERVER=/home/binarin/mirantis-workspace/rabbit/upstream/scripts/rabbitmq-server
RABBITMQCTL="$(dirname $RABBITMQ_SERVER)/rabbitmqctl"
ERLANG_SSL=1

run-ctl() {
    ERL_LIBS="$(erl-libs)" \
    RABBITMQ_CTL_ERL_ARGS="$(erl-args)" \
    $RABBITMQCTL "$@"
}

run-server-binary() {
    ERL_LIBS=$(erl-libs) \
    RABBITMQ_CTL_ERL_ARGS="$(erl-args)" \
    RABBITMQ_SERVER_ADDITIONAL_ERL_ARGS="$(erl-args)" \
    $RABBITMQ_SERVER "$@"
}

erl-libs() {
    local deps_dir
    deps_dir="$(dirname $(dirname $RABBITMQ_SERVER))/deps"
    if [[ -d $deps_dir ]]; then
        echo $deps_dir
    fi
}

erl-args() {
    local ssl_path; ssl_path=$(erl -eval 'io:format("~p", [code:lib_dir(ssl, ebin)]),halt().' -noshell)
    local ssl_ca_dir; ssl_ca_dir=$(node-dir ssl-certificates)

    if [[ $ERLANG_SSL -gt 0 ]]; then
        echo "-pa $ssl_path -proto_dist inet_tls"
        echo "-ssl_dist_opt server_certfile $ssl_ca_dir/server/key-and-cert.pem"
        echo "-ssl_dist_opt client_certfile $ssl_ca_dir/client/key-and-cert.pem"

        echo "-ssl_dist_opt server_cacertfile $ssl_ca_dir/cacert.pem"
        echo "-ssl_dist_opt client_cacertfile $ssl_ca_dir/cacert.pem"

        echo "-ssl_dist_opt server_secure_renegotiate true"
        echo "-ssl_dist_opt client_secure_renegotiate true"

        echo "-ssl_dist_opt client_verify verify_peer"
        echo "-ssl_dist_opt server_verify verify_peer"

        local ciphers=DHE-RSA-AES256-SHA256:DHE-DSS-AES256-SHA256:DHE-RSA-AES128-SHA256:DHE-DSS-AES128-SHA256:AES128-SHA256:DHE-RSA-AES256-SHA:DHE-DSS-AES256-SHA:AES256-SHA:DHE-RSA-AES128-SHA:DHE-DSS-AES128-SHA:AES128-SHA

        echo "-ssl_dist_opt client_ciphers $ciphers"
        echo "-ssl_dist_opt server_ciphers $ciphers"
    fi
}

ensure-ssl-certs() {
    set -x
    local dir
    dir=$(node-dir ssl-certificates)

    rm -rf $dir

    prepare-ssl-ca-dir $dir
    ensure-ssl-ca-config $dir
    
    # CA
    openssl req -x509 -config $dir/openssl.conf -newkey rsa:2048 -days 365 \
            -out $dir/cacert.pem -outform PEM -subj /CN=MyTestCA/ -nodes
    openssl x509 -in $dir/cacert.pem -out $dir/cacert.cer -outform DER

    # Server
    mkdir $dir/server
    openssl genrsa -out $dir/server/key.pem 2048
    openssl req -new -key $dir/server/key.pem -out $dir/server/req.pem -outform PEM \
            -subj /CN=$(hostname)/O=server/ -nodes

    # Sign server cert
    openssl ca -config $dir/openssl.conf \
            -in $dir/server/req.pem -out $dir/server/cert.pem \
            -notext -batch -extensions server_ca_extensions

    # Server key/cert bundle
    cat $dir/server/cert.pem $dir/server/key.pem > $dir/server/key-and-cert.pem
    chmod 0600 $dir/server/key-and-cert.pem

    # Client
    mkdir $dir/client
    openssl genrsa -out $dir/client/key.pem 2048
    openssl req -new -key $dir/client/key.pem -out $dir/client/req.pem -outform PEM \
            -subj /CN=$(hostname)/O=client/ -nodes

    # Sign client cert
    openssl ca -config $dir/openssl.conf \
            -in $dir/client/req.pem -out $dir/client/cert.pem \
            -notext -batch -extensions client_ca_extensions

    # Client key/cert bundle
    cat $dir/client/cert.pem $dir/client/key.pem > $dir/client/key-and-cert.pem
    chmod 0600 $dir/client/key-and-cert.pem
}

prepare-ssl-ca-dir() {
    local dir="${1:?}"
    mkdir -p $dir/{certs,private}
    chmod 0700 $dir/private
    echo 01 > $dir/serial
    touch $dir/index.txt
}

base-port-for() {
    local node="${1:?}"
    case $node in
        ssl-1)
            echo 20100;;
        ssl-2)
            echo 20200;;
        ssl-3)
            echo 20300;;
        *)
            echo "Can't find port range for node $node" 1>&2 
            exit 1
            ;;
    esac
}

amqp-port() {
    local base_port="${1:?}"
    echo $(($base_port + 1))
}

dist-port() {
    local base_port="${1:?}"
    echo $(($base_port + 2))
}

amqp-ssl-port() {
    local base_port="${1:?}"
    echo $(($base_port + 3))
}

node-dir() {
    local node_name="${1:?}"
    echo "/tmp/test-rabbit-instances/$node_name"
}

start-fresh-node() {
    local node_name="${1:?}"

    local node_dir
    node_dir="$(node-dir $node_name)"

    reset-node $node_name
    start-node $node_name
}

reset-node() {
    local node_name="${1:?}"
    local node_dir
    node_dir="$(node-dir $node_name)"

    pkill -9 -f "beam.*-sname $node_name@localhost" || true
    reset-node-dir $node_dir
    generate-rabbit-config $node_name
}

reset-node-dir() {
    local node_dir="${1:?}"
    rm -rf "$node_dir"
    mkdir -p $node_dir/{conf.d,log,schema}
}

start-fresh-background-node() {
    local node_name="${1:?}"
    local node_dir; node_dir="$(node-dir $node_name)"
    reset-node $node_name
    start-node $node_name > $node_dir/log/startup_err 2>&1 &
}

start-node() {
    local node_name="${1:?}"

    local node_dir; node_dir="$(node-dir $node_name)"

    set -x
    RABBITMQ_MNESIA_BASE="$node_dir/mnesia" \
    RABBITMQ_LOG_BASE="$node_dir/log" \
    RABBITMQ_CONFIG_FILE="$node_dir/rabbitmq.config" \
    RABBITMQ_NODENAME="$node_name@localhost" \
    RABBITMQ_SCHEMA_DIR="$node_dir/schema" \
    run-server-binary
}

generate-rabbit-config() {
    local node_name="${1:?}"
    local node_dir; node_dir="$(node-dir $node_name)"
    local base_port; base_port="$(base-port-for $node_name)"

    set-config $node_dir \
               kernel inet_dist_listen_min $(dist-port $base_port) \
               kernel inet_dist_listen_max $(dist-port $base_port) \
               rabbit tcp_listeners "[$(amqp-port $base_port)]"

    set-ssl-options $node_name

    render-config $node_dir > $node_dir/rabbitmq.config
}

set-ssl-options() {
    local node_name="${1:?}"
    local node_dir; node_dir="$(node-dir $node_name)"
    local base_port; base_port="$(base-port-for $node_name)"
    local ssl_ca_dir; ssl_ca_dir=$(node-dir ssl-certificates)

    set-config $node_dir \
               rabbit ssl_listeners "[$(amqp-ssl-port $base_port)]" \
               rabbit ssl_options "$(cat <<EOF
[{cacertfile, "$ssl_ca_dir/cacert.pem"}
,{certfile, "$ssl_ca_dir/server/cert.pem"}
,{keyfile, "$ssl_ca_dir/server/key.pem"}
,{verify, verify_peer}
,{fail_if_no_peer_cert,true}
,{ciphers, "DHE-RSA-AES256-SHA256:DHE-DSS-AES256-SHA256:DHE-RSA-AES128-SHA256:DHE-DSS-AES128-SHA256:AES128-SHA256:DHE-RSA-AES256-SHA:DHE-DSS-AES256-SHA:AES256-SHA:DHE-RSA-AES128-SHA:DHE-DSS-AES128-SHA:AES128-SHA"}
]
EOF
)"

}

set-config() {
    local node_dir="${1:?}"; shift
    local section
    local param
    local value

    while [[ $# -gt 0 ]]; do
        section="$1"; param="$2"; value="$3"; shift 3
        mkdir -p $node_dir/conf.d/$section
        echo "  ,{$param, $value}" > $node_dir/conf.d/$section/$param
    done
}

wait-nodes() {
    local node
    for node in "$@"; do
        wait-node $node
    done
}

wait-node() {
    local try_no
    for try_no in $(seq 1 10); do
        if run-ctl -n "$1@localhost" status > /dev/null 2>&1 ; then
            return 0
        fi
        sleep 1
    done
    return 1
}

join-nodes() {
    local first_node="${1:?}"; shift
    local node_name
    for node_name in "$@"; do
        join-node $first_node $node_name
    done
}

join-node() {
    local join_to="${1:?}"
    local node_name="${2:?}"
    run-ctl -n $node_name@localhost stop_app
    run-ctl -n $node_name@localhost join_cluster $join_to@localhost
    run-ctl -n $node_name@localhost start_app
}

render-config() {
    local node_dir="${1:?}"
    local section
    local maybe_file

    echo "[{make_next_comma_safe_dummy_app,"
    echo "  []"
    echo " }"

    for section in rabbit kernel; do
        echo -e ",{$section,\n  [{make_next_comma_safe_dummy_param, true}"
        for maybe_file in $node_dir/conf.d/$section/*; do
            if [[ ! -e $maybe_file ]]; then
                break
            fi
            cat $maybe_file
        done
        echo "]}"
    done
    echo "]."
}

ensure-ssl-ca-config() {
    local dir="${1:?}"
    cat <<EOF > $dir/openssl.conf
[ ca ]
default_ca = testca

[ testca ]
dir = $dir
certificate = $dir/cacert.pem
database = $dir/index.txt
new_certs_dir = $dir/certs
private_key = $dir/private/cakey.pem
serial = $dir/serial

default_crl_days = 7
default_days = 365
default_md = sha256

policy = testca_policy
x509_extensions = certificate_extensions

[ testca_policy ]
commonName = supplied
stateOrProvinceName = optional
countryName = optional
emailAddress = optional
organizationName = optional
organizationalUnitName = optional

[ certificate_extensions ]
basicConstraints = CA:false

[ req ]
default_bits = 2048
default_keyfile = $dir/private/cakey.pem
default_md = sha256
prompt = yes
distinguished_name = root_ca_distinguished_name
x509_extensions = root_ca_extensions

[ root_ca_distinguished_name ]
commonName = hostname

[ root_ca_extensions ]
basicConstraints = CA:true
keyUsage = keyCertSign, cRLSign

[ client_ca_extensions ]
basicConstraints = CA:false
keyUsage = digitalSignature
extendedKeyUsage = 1.3.6.1.5.5.7.3.2

[ server_ca_extensions ]
basicConstraints = CA:false
keyUsage = keyEncipherment
extendedKeyUsage = 1.3.6.1.5.5.7.3.1
EOF
}

