#!/bin/sh

# This script fetches the RabbitMQ client. It is called as a rebar hook when
# you do `rebar get-deps'.
#
# It is not a simple rebar dependency, unfortunately.

VERSION=3.3.5
BASE_URL=http://www.rabbitmq.com/releases/rabbitmq-erlang-client
for LIB in rabbit_common amqp_client ; do
    if [ ! -e deps/$LIB ]; then
        FILE=$LIB-$VERSION.ez
        URL=$BASE_URL/v$VERSION/$FILE
        wget -O $FILE -nc $URL && unzip $FILE && mv $LIB-$VERSION deps/$LIB
        rm $FILE
    fi
done
