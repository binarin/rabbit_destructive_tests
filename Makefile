PROJECT = rabbit_destructive_tests
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.0.1

dep_lager = git https://github.com/basho/lager 2.1.0
dep_lager_commit = 2.1.0
BUILD_DEPS += lager

include erlang.mk

