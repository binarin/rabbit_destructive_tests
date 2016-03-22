Destructive testing of rabbitmq
===============================

Requirements
------------

Password-less sudo for `tc` command - for simulating slow network on loopback interface.

Running tests
-------------

Run a single CT suite with checked-out version of rabbit:

    RABBITMQ_GIT=/home/binarin/mirantis-workspace/rabbit/rabbitmq-server-parallel-listing  make ct-lots_of_channels

E.g. this will test that listing significant amounts of items through
slow network (1ms delay) doesn't have big negative impact.

