FROM debian
ADD ./prepare.sh /tmp/prepare.sh
RUN /tmp/prepare.sh
ADD ./rabbit_under_test /tmp/rabbit_under_test
ADD ./rabbit_destructive_tests /tmp/rabbit_destructive_tests
