PROJECT = bb_rpc_driver
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.0.1

ERLC_OPTS += +'{parse_transform, lager_transform}'
DEPS += lager basho_bench

dep_basho_bench = git https://github.com/basho/basho_bench 5c14587eae755852d8d208b85a6c806c0a311cd1
dep_lager = git https://github.com/basho/lager 2.1.0
dep_lager_commit = 2.1.0

include ../../erlang.mk
