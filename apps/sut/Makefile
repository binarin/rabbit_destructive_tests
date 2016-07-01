PROJECT = sut
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.0.1

DEPS += lager amqp_client eep

dep_lager = git https://github.com/basho/lager 2.1.0
dep_lager_commit = 2.1.0

include ../../erlang.mk

ERLC_OPTS += +'{parse_transform, lager_transform}'
