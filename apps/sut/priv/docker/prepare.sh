#!/bin/sh
set -eu

cat <<EOF > /etc/apt/sources.list
deb http://httpredir.debian.org/debian jessie main contrib non-free
deb-src http://httpredir.debian.org/debian jessie main contrib non-free
deb http://httpredir.debian.org/debian jessie-updates main contrib non-free
deb http://security.debian.org jessie/updates main contrib non-free
EOF

export DEBIAN_FRONTEND=noninteractive
apt-get update
apt-get -y build-dep rabbitmq-server
apt-get -y install socat less dnsutils bind9utils iptables
