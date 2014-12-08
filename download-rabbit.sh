#!/bin/sh

# This script fetches the RabbitMQ client. It is called as a rebar hook when
# you do `rebar get-deps'.
#
# It is not a simple rebar dependency, unfortunately.

VERSION=3.3.5
BASE_URL=http://www.rabbitmq.com/releases/rabbitmq-erlang-client
for LIB in rabbit_common amqp_client ; do

    ARCHIVE_DIR=$REBAR_DEPS_DIR/$LIB-$VERSION
    ARCHIVE=$ARCHIVE_DIR.ez
    LIB_DIR=$REBAR_DEPS_DIR/$LIB

    if [ ! -e $LIB_DIR ]; then
        URL=$BASE_URL/v$VERSION/$(basename $ARCHIVE)
        wget -O $ARCHIVE -nc $URL && unzip $ARCHIVE -d $REBAR_DEPS_DIR && mv $ARCHIVE_DIR $LIB_DIR
        rm $ARCHIVE
    fi
done
