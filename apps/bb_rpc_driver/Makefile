PROJECT = bb_rpc_driver
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.0.1

dep_basho_bench = git https://github.com/basho/basho_bench 5c14587eae755852d8d208b85a6c806c0a311cd1

ERLC_OPTS += +'{parse_transform, lager_transform}'
DEPS += lager basho_bench

IGNORE_DEPS += node_package folsom riakc mochiweb katja snappy lz4 semver uuid pooler

include ../../erlang.mk
